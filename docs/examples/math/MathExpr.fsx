#load "Include.fsx"

open System
open System.Reflection

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns

open MathNet.Numerics
open MathNet.Symbolics
open ExpressionPatterns
open MathNet.Symbolics.Operators
open Sylvester

let rec toQuotation<'t> (expr: Expression) (vars: Var list) =
    let cs x = cos x
    let value = function
        | Value.Approximation a -> Expr.Value a.RealValue |> Some
        | Value.NegativeInfinity -> Expr.Value Double.NegativeInfinity |> Some
        | Value.PositiveInfinity -> Expr.Value System.Double.PositiveInfinity |> Some
        | Value.Number n -> n |> float |> Expr.Value |> Some
        | _ -> None
    let constant = function
        | E -> Expr.Value Constants.E  |> Some 
        | Pi -> Expr.Value Constants.Pi |> Some
        | _ -> None
    let argName = function |Symbol(n) -> n
    let getParam p =
        List.fold(
                fun x (y : Var) ->
                    match x with
                    | None when y.Name = (argName p) -> Some y
                    | Some(v) -> Some v
                    | _ -> None
                ) None vars
    let rec getMethodInfo = function
        | Call(None, methodInfo, _) -> methodInfo
        | Lambda(_, expr) -> getMethodInfo expr
        | _ -> failwith "Expression is not a function."

    let add (a:Expr) (b:Expr) = 
        do if a.Type <> b.Type then failwithf "The type of the LHS:%A is not the type of the RHS: %A " a.Type b.Type
        match a.Type.Name with
        | "Int32" -> <@@ (%%a:int) + (%%b:int) @@>
        | "Int64" -> <@@ (%%a:int64) + (%%b:int64) @@>
        | "Double" -> <@@ (%%a:float) + (%%b:float) @@>
        | "Single" -> <@@ (%%a:float32) + (%%b:float32) @@>
        | "BigRational" -> <@@ (%%a:BigRational) + (%%b:BigRational) @@>
        | "BigInteger" -> <@@ (%%a:BigInteger) + (%%b:BigInteger) @@>
        | n -> failwithf "Addition operation for type %A not supported." a.Type 
    
    let mul (a:Expr) (b:Expr) = 
        do if a.Type <> b.Type then failwithf "The type of the LHS:%A is not the type of the RHS: %A " a.Type b.Type
        match a.Type.Name with
        | "Int32" -> <@@ (%%a:int) * (%%b:int) @@>
        | "Int64" -> <@@ (%%a:int64) * (%%b:int64) @@>
        | "Double" -> <@@ (%%a:float) * (%%b:float) @@>
        | "Single" -> <@@ (%%a:float32) * (%%b:float32) @@>
        | "BigRational" -> <@@ (%%a:BigRational) * (%%b:BigRational) @@>
        | "BigInteger" -> <@@ (%%a:BigInteger) * (%%b:BigInteger) @@>
        | n -> failwithf "Multiplication operation for type %A not supported." a.Type
    
    let sub (a:Expr) (b:Expr) = 
        do if a.Type <> b.Type then failwithf "The type of the LHS:%A is not the type of the RHS: %A " a.Type b.Type
        match a.Type.Name with
        | "Int32" -> <@@ (%%a:int) - (%%b:int) @@>
        | "Int64" -> <@@ (%%a:int64) - (%%b:int64) @@>
        | "Double" -> <@@ (%%a:float) - (%%b:float) @@>
        | "Single" -> <@@ (%%a:float32) - (%%b:float32) @@>
        | "BigRational" -> <@@ (%%a:BigRational) - (%%b:BigRational) @@>
        | "BigInteger" -> <@@ (%%a:BigInteger) - (%%b:BigInteger) @@>
        | n -> failwithf "Subtraction operation for type %A not supported." a.Type
    
    let div (a:Expr) (b:Expr) = 
        do if a.Type <> b.Type then failwithf "The type of the LHS:%A is not the type of the RHS: %A " a.Type b.Type
        match a.Type.Name with
        | "Int32" -> <@@ (%%a:int) / (%%b:int) @@>
        | "Int64" -> <@@ (%%a:int64) / (%%b:int64) @@>
        | "Double" -> <@@ (%%a:float) / (%%b:float) @@>
        | "Single" -> <@@ (%%a:float32) / (%%b:float32) @@>
        | "BigRational" -> <@@ (%%a:BigRational) / (%%b:BigRational) @@>
        | "BigInteger" -> <@@ (%%a:BigInteger) / (%%b:BigInteger) @@>
        | n -> failwithf "Addition operation for type %A not supported." a.Type

    let rec convertExpr : Expression -> Expr option = 
        function
        | Identifier(sym) -> (getParam sym) |> Option.map (fun x -> Expr.Var(x))
        | Values.Value v -> value v
        | Constant c -> constant c
        | Sum(xs) ->
            let summands = List.map convertExpr xs
            List.fold (Option.map2 add) (value Value.zero) summands
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
                | Log   -> getMethodInfo <@ Math.Log10 @> |> Some
                | Exp  -> getMethodInfo <@ Math.Exp @> |> Some
                //| Abs  -> getMethodInfo <@ Math.Abs @> |> Some
                | _    -> None
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
                Option.map (fun x -> Expr.Call(getMethodInfo <@ Math.Sqrt @>, [x])) a
            else
                let a = convertExpr x
                let b = convertExpr (Power(n, minusOne))
                Option.map2 (fun x y -> Expr.Call (getMethodInfo <@ Math.Pow @>, x::y::[])) a b
        | Power(Constant E, y) ->
            let exponent = convertExpr y
            Option.map (fun x -> Expr.Call(getMethodInfo <@ Math.Exp @>, [x])) exponent
        | Power(x, y) ->
            let baseE = convertExpr x
            let exponE = convertExpr y
            Option.map2 (fun x y -> Expr.Call(getMethodInfo <@ Math.Sqrt @>, x::y::[])) baseE exponE
        | _ -> None
    and compileFraction = function
        | Product(xs) ->
            let prods = List.map convertExpr xs
            List.fold (Option.map2 mul) (value Value.one) prods
        | x -> convertExpr x

    convertExpr expr

let x = var<BigRational>
let j = BigRational.FromInt(2) + x

let q = <@ x @>
let v = [Var("x", typeof<BigInteger>)]

toQuotation  
 