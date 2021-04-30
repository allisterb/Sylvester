namespace Sylvester

open System
open System.Numerics
open System.Reflection

open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns

open MathNet.Numerics
open MathNet.Symbolics
open MathNet.Symbolics.ExpressionPatterns
open MathNet.Symbolics.Operators

type MathNetExpr = Expression

module MathNetExpr =
    let rec fromQuotation (q:Expr) : Expression =
        match q with
        | SpecificCall <@@ ( + ) @@> (_, _, [xt; yt]) -> (fromQuotation xt) + (fromQuotation yt)
        | SpecificCall <@@ ( - ) @@> (_, _, [xt; yt]) -> (fromQuotation xt) - (fromQuotation yt)
        | SpecificCall <@@ ( ~- ) @@> (_, _, [xt]) -> -(fromQuotation xt)
        | SpecificCall <@@ ( ~+ ) @@> (_, _, [xt]) -> +(fromQuotation xt)
        | SpecificCall <@@ ( * ) @@> (_, _, [xt; yt]) -> (fromQuotation xt) * (fromQuotation yt)
        | SpecificCall <@@ ( / ) @@> (_, _, [xt; yt]) -> (fromQuotation xt) / (fromQuotation yt)
        | SpecificCall <@@ ( ** ) @@> (_, _, [xt; yt]) -> (fromQuotation xt) ** (fromQuotation yt)
        | SpecificCall <@@ Math.Pow @@> (_, _, [xt; yt]) -> Expression.Pow(fromQuotation xt, fromQuotation yt)
        | SpecificCall <@@ Numbers.real @@> (_, _, Int32 n::[]) -> Expression.Real (float n)
        | SpecificCall <@@ Numbers.real @@> (_, _, e::[]) -> fromQuotation e
        
        | Call(None, Op "FromInt32" ,Value(v, _)::[]) -> fromInt32 (v :?> int)
        | Call(None, Op "Abs" ,v::[]) -> Expression.Abs (fromQuotation v)
        | ValueWithName(_, _, n) -> Identifier (Symbol n) 
        | Var x -> Identifier (Symbol x.Name)
        | PropertyGet (_, info, _) -> Identifier (Symbol info.Name)
        | Int16 k -> fromInt32 (int k)
        | Int32 k -> fromInt32 k
        | Int64 k -> fromInt64 k
        | UInt16 k -> fromInt32 (int k)
        | UInt32 k -> fromInt64 (int64 k)
        | UInt64 k -> fromInteger (BigInteger k)
        | Double d -> Expression.Real d
        | Single d -> Expression.Real (float d)
        | Rational r -> Number (BigRational.FromBigIntFraction(r.Numerator, r.Denominator))
        | Value(v, t) when t = typeof<Complex> -> Expression.Complex (v :?> Complex)
        | Let (_, _, t) -> fromQuotation t
        | Lambda (_, t) -> fromQuotation t
        | _ -> failwithf "Expression %A is not currently supported." <| q

    let fromEqualityQuotation = 
        function
        | SpecificCall <@@ ( = ) @@> (_, _, [l; r]) -> fromQuotation l, fromQuotation r
        | expr -> failwithf "The expression %s is not an equality." <| src expr

    let rec toQuotation<'t> (vars: Var list) (expr: Expression)  =    
        let rec numerator = function
            | NegPower _ -> one
            | Product ax -> product <| List.map numerator ax
            | z -> z
        
        let rec denominator = function
            | NegPower (r, p) -> r ** -p
            | Product ax -> product <| List.map denominator ax
            | _ -> one

        let value = function
            | Value.Approximation a -> 
                match a with
                | Real r -> 
                    let v = r |> Expr.Value
                    if typeof<'t> = typeof<float> then Some v else Some(Expr.Value(Convert.ChangeType(v, typeof<'t>) :?> 't))
                | Complex c -> 
                    let v = c |> Expr.Value
                    if typeof<'t> = typeof<complex> then Some v else Some(Expr.Value(Convert.ChangeType(v, typeof<'t>) :?> 't))   
            | Value.NegativeInfinity -> Expr.Value Double.NegativeInfinity |> Some
            | Value.PositiveInfinity -> Expr.Value System.Double.PositiveInfinity |> Some
            | Value.Number n -> 
                if typeof<'t> = typeof<int> && n.IsInteger then 
                    Expr.Value(int n.Numerator) |> Some
                else if typeof<'t> = typeof<Rational> then 
                    Expr.Value(Rational(n.Numerator, n.Denominator)) |> Some
                else
                    Expr.Value(Convert.ChangeType(float n, typeof<'t>) :?> 't) |> Some
            | v -> failwithf "Could not convert the value %A to an Expr." v
        let constant = function
            | E -> Expr.Value Constants.E  |> Some 
            | Pi -> Expr.Value Constants.Pi |> Some
            | c -> failwithf "Could not convert the constant %A to an Expr." c
        let argName = function |Symbol(n) -> n
        
        let getParam p =
            List.fold(
                    fun x (y : Var) ->
                        match x with
                        | None when y.Name = (argName p) -> Some y
                        | Some v -> Some v
                        | None -> None //failwithf "Did not find a matching var for %A." p
                    ) None vars
        
        let getMethodInfo = expand >> getFuncInfo
        
        let add (a:Expr) (b:Expr) = let op = addOp.[a.Type.Name] in Expr.Call(op, a::b::[])
    
        let mul (a:Expr) (b:Expr) = let op = mulOp.[a.Type.Name] in Expr.Call(op, a::b::[])
                
        let sub (a:Expr) (b:Expr) = let op = subOp.[a.Type.Name] in Expr.Call(op, a::b::[])
        
        let div (a:Expr) (b:Expr) = let op = divOp.[a.Type.Name] in Expr.Call(op, a::b::[])
            
        let rec convertExpr : Expression -> Expr option = 
            function 
            | Identifier(Symbol "One") -> Expr.Value(Rational.One) |> Some
            | Identifier(sym) -> (getParam sym) |> Option.map (fun x -> Expr.Var(x))
            | Values.Value v -> value v
            | Constant c -> constant c
            | Sum(xs) ->
                let summands = List.map convertExpr xs
                summands.Tail |> List.fold (Option.map2 add) summands.Head 
            | Product(_) as p ->
                let n = numerator p
                let d = denominator p
                if isOne d then
                    compileFraction n
                else
                    let nExp = compileFraction n
                    let dExp = compileFraction d
                    Option.map2 div nExp dExp
            | Function (func, par) ->
                let convertFunc : Function -> MethodInfo option = function
                    | Sin  -> getMethodInfo <@ Math.Sin @> |> Some
                    | Cos  -> getMethodInfo <@ Math.Cos @> |> Some
                    | Tan  -> getMethodInfo <@ Math.Tan @> |> Some
                    //| Csc  -> getMethodInfo <@ Math.Csc @> |> Some
                    //| Sec  -> getMethodInfo <@ Math.Sec @> |> Some
                    //| Cot -> getMethodInfo <@ Math.Cot @> |> Some
                    | Sinh -> getMethodInfo <@ Math.Sinh @> |> Some
                    | Cosh -> getMethodInfo <@ Math.Cosh @> |> Some
                    | Tanh -> getMethodInfo <@ Math.Tanh @> |> Some
                    //| Csch -> getMethodInfo <@ Math.Csch @> |> Some
                    //| Sech  -> Some (mathCall1 "Sech")
                    //| Coth  -> Some (mathCall1 "Coth")
                    | Asin -> getMethodInfo <@ Math.Asin @> |> Some
                    | Acos -> getMethodInfo <@ Math.Acos @> |> Some                
                    | Atan -> getMethodInfo <@ Math.Atan @> |> Some
                    //| Acsc -> Some (mathCall1 "Acsc")
                    //| Asec -> Some (mathCall1 "Asec")
                    //| Acot -> Some (mathCall1 "Acot")
                    //| Asinh -> Some (mathCall1 "Asinh")
                    //|Acosh -> Some (mathCall1 "Acosh")
                    //| Atanh -> Some (mathCall1 "Atanh")
                    //| Acsch -> Some (mathCall1 "Acsch")
                    //| Asech -> Some (mathCall1 "Asech")
                    //| Acoth -> Some (mathCall1 "Acoth")
                    //| AiryAi -> Some (mathCall1 "AiryAi")
                    //| AiryAiPrime -> Some (mathCall1 "AiryAiPrime")
                    //| AiryBi -> Some (mathCall1 "AiryBi")
                    //| AiryBiPrime -> Some (mathCall1 "AiryBiPrime")
                    | Ln   -> getMethodInfo <@ Math.Log @> |> Some
                    //| Log   -> getMethodInfo <@ Math.Log10 @> |> Some
                    | Exp  -> getMethodInfo <@ Math.Exp @> |> Some
                    | Abs  ->  absOp.[typeof<'t>.Name] |> Some 
                    //    let a = Math.Abs
                    //    let b = a.GetType
                    | e    -> failwithf "Could not convert function %A to quotation." e
                let f = convertFunc func
                let e = convertExpr par
                Option.map2 (fun x y -> Expr.Call(x, [y])) f e
            | PosIntPower(x, Number(y)) ->
                let basis = convertExpr x
                let rec exponentiate (power : BigRational) exp  =
                    if  power.Numerator.IsEven then
                        let newBasis = mul exp exp
                        exponentiate (power / (BigRational.FromInt(2))) newBasis
                    elif power = 1N then
                        exp
                    else
                        let newBasis = exponentiate (power - BigRational.One) exp
                        mul exp newBasis
                Option.map (exponentiate y) basis
            | Power(x, minusOne) when minusOne = Expression.MinusOne ->
                let a = convertExpr x
                Option.map2 div (value Value.one) a
            | Power (x, Power(n, minusOne)) when minusOne = Expression.MinusOne ->
                let a = convertExpr x
                let b = convertExpr (Power(n, minusOne))
                if n = Operators.two then
                    Option.map (fun x -> Expr.Call((getMethodInfo <@ Math.Sqrt @>), [x])) a
                else
                    let a = convertExpr x
                    let b = convertExpr (Power(n, minusOne))
                    Option.map2 (fun x y -> Expr.Call ((<@ Math.Pow @> |> expand |> getFuncInfo ), x::y::[])) a b
            | Power(Constant E, y) ->
                let exponent = convertExpr y
                Option.map (fun x -> Expr.Call((getMethodInfo <@ Math.Exp @>), [x])) exponent
            | Power(x, y) ->
                let baseE = convertExpr x
                let exponE = convertExpr y
                Option.map2 (fun x y -> Expr.Call(<@ Math.Pow @> |> expand |> getFuncInfo, x::y::[])) baseE exponE
            | expr -> failwithf "Did not convert %A." expr
        and compileFraction = 
            function
            | Product(xs) ->
                let prods = List.map convertExpr xs in
                prods.Tail |> List.fold (Option.map2 mul) prods.Head
            | x -> convertExpr x

        convertExpr expr

    let toQuotation'<'t> (vars: Var list) (expr: Expression) =
        match toQuotation<'t> vars expr with
        | Some e -> e |> expand''<'t>
        | None -> failwithf "Failed to convert expression %s to quotation" (Infix.format expr)

    let toIdentifier(v:Var) = 
        v.Name  
        |> Symbol 
        |> Identifier

    let callUnary<'t> (op:Expression -> Expression) (x:Expr<'t>) = 
        x
        |> fromQuotation 
        |> op 
        |> toQuotation'<'t> (x |> get_vars)

    let callBinary (op:Expression -> Expression -> Expression) (p:Expression) (x:Expr) = 
        x 
        |> fromQuotation 
        |> op p
        |> toQuotation (x |> get_vars)
        |> Option.get
