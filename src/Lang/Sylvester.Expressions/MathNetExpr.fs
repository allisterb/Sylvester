namespace Sylvester

open System
open System.IO
open System.Text
open System.Numerics
open System.Reflection

open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns

open FParsec

open MathNet.Numerics
open MathNet.Symbolics
open MathNet.Symbolics.ExpressionPatterns
open MathNet.Symbolics.Operators

type MathNetExpr = MathNet.Symbolics.Expression

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
        
        | SpecificCall <@@ Numbers.real @@> (_, _, Int32 n::[]) -> Expression.Real (float n)
        | SpecificCall <@@ Numbers.real @@> (_, _, e::[]) -> fromQuotation e
        | Call(None, Op "FromInt32", Value(v, _)::[]) when q.Type = typeof<Rational> -> fromInt32 (v :?> int)
        | Call(None, Op "ToInt", Double v::[]) -> fromInt32 ((int) v)
        | Call(None, Op "ToInt", e::[]) -> fromQuotation e
        | Call(None, Op "ToDouble", Int32 v::[]) -> fromInt32 v
        | Call(None, Op "ToDouble", e::[]) -> fromQuotation e
        | Call(None, Op "ToInt", e::[]) -> fromQuotation e
        | Call(None, Op "FromZero", _) when q.Type = typeof<Rational> -> Number(BigRational.Zero)
        | Call(None, Op "FromOne", _) when q.Type = typeof<Rational> -> Number(BigRational.One)
        
        | Call(None, Op "Abs", v::[]) -> Expression.Abs (fromQuotation v)
        | Call(None, Op "Sqrt", v::[]) -> Expression.Root(Number(BigRational.FromInt 2), (fromQuotation v))
        | Call(None, Op "Sin", v::[]) -> Expression.Sin(fromQuotation v)
        | Call(None, Op "Cos", v::[]) -> Expression.Cos(fromQuotation v)
        | Call(None, Op "Tan", v::[]) -> Expression.Tan(fromQuotation v)
        | Call(None, Op "Sinh", v::[]) -> Expression.Sinh(fromQuotation v)
        | Call(None, Op "Cosh", v::[]) -> Expression.Cosh(fromQuotation v)
        | Call(None, Op "Tanh", v::[]) -> Expression.Tanh(fromQuotation v)

        | Call(None, Op "factorial", v::[]) -> Expression.Factorial (fromQuotation v)
        | Call(None, Op "Factorial", v::[]) -> Expression.Factorial (fromQuotation v)
        | Call(None, Op "binomial_coeff", n::r::[]) -> 
            let n',r' = fromQuotation n, fromQuotation r in
            (Expression.Factorial n') / (Expression.Factorial(r') * (Expression.Factorial(n' - r')))
        
        | PropertyGet(None, Prop "pi", []) -> Expression.Pi

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
        | Natural n -> Number (BigRational.FromBigInt(n.IntVal))
        | Value(v, t) when t = typeof<Complex> -> Expression.Complex (v :?> Complex)
        
        | Let (_, _, t) -> fromQuotation t
        | Lambda (_, t) -> fromQuotation t
        | _ -> failwithf "Expression %A is not currently supported." <| q

    let fromEqualityQuotation = 
        function
        | SpecificCall <@@ ( = ) @@> (_, _, [l; r]) -> fromQuotation l, fromQuotation r
        | expr -> failwithf "The expression %s is not an equality." <| src expr

    let rec _toQuotation<'t> (vars: Var list) (expr: Expression)  =    
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
        
        let getPropInfo = expand >> getPropertyInfo

        let add (a:Expr) (b:Expr) = let op = addOp.[a.Type.Name] in Expr.Call(op, a::b::[])
    
        let mul (a:Expr) (b:Expr) = let op = mulOp.[a.Type.Name] in Expr.Call(op, a::b::[])
                
        let sub (a:Expr) (b:Expr) = let op = subOp.[a.Type.Name] in Expr.Call(op, a::b::[])
        
        let div (a:Expr) (b:Expr) = let op = divOp.[a.Type.Name] in Expr.Call(op, a::b::[])
       
        let rec convertExpr : Expression -> Expr option = 
            function 
            | Identifier(Symbol "One") -> Expr.Value(Rational.One) |> Some
            | Identifier(sym) -> (getParam sym) |> Option.map (fun x -> Expr.Var(x))
            | Values.Value v -> value v
            | Constant (Constant.Pi) -> let p = getPropertyInfo <@ pi @> in Expr.PropertyGet p |> Some
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
                    | Exp  -> getMethodInfo <@ Math.Exp @> |> Some
                    | Abs  -> absOp.[typeof<'t>.Name] |> Some 
                    | Sin  -> sinOp.[typeof<'t>.Name]  |> Some
                    | Cos  -> cosOp.[typeof<'t>.Name]  |> Some
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
                    Option.map (fun x -> call_sqrt x) a
                else
                    let a = convertExpr x
                    let b = convertExpr (Power(n, minusOne))
                    let t = a.Value.Type
                    Option.map2 (fun x y -> if t = typeof<int> then call_pown x y else call_pow x y) a b
            | Power(Constant E, y) ->
                let exponent = convertExpr y
                Option.map (fun x -> Expr.Call((getMethodInfo <@ Math.Exp @>), [x])) exponent
            | Power(x, y) ->
                let a = convertExpr x
                let b = convertExpr y
                let t = a.Value.Type
                Option.map2 (fun j k -> if t = typeof<int> then call_pown j k else call_pow j k) a b
            | expr -> failwithf "Did not convert %A." expr
        and compileFraction = 
            function
            | Product(xs) ->
                let prods = List.map convertExpr xs in
                prods.Tail |> List.fold (Option.map2 mul) prods.Head
            | x -> convertExpr x

        convertExpr expr

    let toQuotation<'t> (vars: Var list) (expr: Expression) =
        match _toQuotation<'t> vars expr with
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
        |> toQuotation<'t> (x |> get_vars)

    let callBinary (op:Expression -> Expression -> Expression) (p:Expression) (x:Expr) = 
        x 
        |> fromQuotation 
        |> op p
        |> _toQuotation (x |> get_vars)
        |> Option.get

module MathNetExprParser =

    type 'a parser = Parser<'a, unit>

    let ws = spaces
    let str_ws s = pstring s .>> ws
    let parens p = between (str_ws "(") (str_ws ")") p |>> VisualExpression.Parenthesis
    let abs p = between (str_ws "|") (str_ws "|") p |>> VisualExpression.Abs

    let integer : BigInteger parser =
        let options = NumberLiteralOptions.None
        numberLiteral options "number" .>> ws
        |>> fun num -> BigInteger.Parse(num.String)

    let number : VisualExpression parser =
        let options =
            NumberLiteralOptions.AllowFraction
            ||| NumberLiteralOptions.AllowFractionWOIntegerPart
            ||| NumberLiteralOptions.AllowInfinity
            ||| NumberLiteralOptions.AllowExponent
        numberLiteral options "number" .>> ws
        |>> fun num ->
            if num.IsInfinity then VisualExpression.Infinity
            elif num.IsInteger then BigInteger.Parse(num.String) |> VisualExpression.PositiveInteger
            else VisualExpression.PositiveFloatingPoint(float num.String)

    let symbolName : string parser =
        let isSymbolFirstChar c = isLetter c
        let isSymbolChar c = isLetter c || isDigit c || c = '_'
        many1Satisfy2L isSymbolFirstChar isSymbolChar "symbol" .>> ws

    let identifier : VisualExpression parser =
        let isMathChar = function | '\u03C0' | '\u221E' | '\u29DD' -> true | _ -> false
        let isIdentifierFirstChar c = isLetter c || isMathChar c
        let isIdentifierChar c = isLetter c || isDigit c || isMathChar c || c = '_'
        many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier" .>> ws
        |>> function // differentating between constants and identifiers
            | "pi" | "\u03C0" -> VisualExpression.RealPi
            | "e" -> VisualExpression.RealE
            | "oo" | "inf" | "\u221E" -> VisualExpression.Infinity // 'oo' from sympy
            | "\u29DD" -> VisualExpression.ComplexInfinity
            | "j" -> VisualExpression.ComplexI
            | id -> VisualExpression.Symbol id

    let expression : VisualExpression parser =

        let opp = OperatorPrecedenceParser<VisualExpression,unit,unit>()
        let expr = opp.ExpressionParser

        let functionArgs = sepBy expr (str_ws ",") |> between (str_ws "(") (str_ws ")")

        let functionTerm = symbolName .>>. functionArgs |>> function
            | f, args -> VisualExpression.Function (f, BigInteger.One, args)

        let functionPowerTerm = symbolName .>>. (str_ws "^" >>. integer) .>>. functionArgs |>> function
            | (f, power), args -> VisualExpression.Function (f, power, args)

        let sqrtTerm = str_ws "sqrt" >>. (between (str_ws "(") (str_ws ")") expr) |>> function
            | arg -> VisualExpression.Root (arg, bigint 2)

        let powTerm = str_ws "pow" >>. functionArgs |>> function
            | [VisualExpression.Negative _ as a; b]
            | [VisualExpression.Sum _ as a; b]
            | [VisualExpression.Product _ as a; b]
            | [VisualExpression.Fraction _ as a; b]
            | [VisualExpression.Power _ as a; b]
            | [VisualExpression.Function _ as a; b]
                -> VisualExpression.Power (VisualExpression.Parenthesis a, b)
            | [a; b] -> VisualExpression.Power (a, b)
            | _ -> failwith "Pow expects exactly two arguments"

        let term =
            number <|> parens expr <|> abs expr
            <|> attempt sqrtTerm <|> attempt powTerm
            <|> attempt functionTerm <|> attempt functionPowerTerm
            <|> identifier

        let sum a b = match a, b with | VisualExpression.Sum ax, b -> VisualExpression.Sum (ax @ [b]) | a, b -> VisualExpression.Sum [a; b]
        let product a b = match a, b with | VisualExpression.Product ax, b -> VisualExpression.Product (ax @ [b]) | a, b -> VisualExpression.Product [a; b]
        let fraction a b =
            let patchParanthesis = function
                | VisualExpression.Parenthesis (VisualExpression.Fraction _) as x -> x
                | VisualExpression.Parenthesis x -> x
                | x -> x
            VisualExpression.Fraction (patchParanthesis a, patchParanthesis b)

        opp.TermParser <- term
        opp.AddOperator(InfixOperator("+", ws, 1, Associativity.Left, sum))
        opp.AddOperator(InfixOperator("-", ws, 1, Associativity.Left, fun a b -> sum a (VisualExpression.Negative b)))
        opp.AddOperator(InfixOperator("*", ws, 2, Associativity.Left, product))
        opp.AddOperator(InfixOperator("/", ws, 2, Associativity.Left, fraction))
        opp.AddOperator(InfixOperator("^", ws, 3, Associativity.Right, fun a b -> VisualExpression.Power (a, b)))
        opp.AddOperator(PrefixOperator("+", ws, 4, true, id))
        opp.AddOperator(PrefixOperator("-", ws, 4, true, VisualExpression.Negative))
        expr

    let parser : VisualExpression parser = ws >>. expression .>> eof
   
    let list_parser :VisualExpression list parser = ws >>. pstring "[" >>. sepBy expression (pstring ",") .>> pstring "]" .>> eof
    
    let list_list_parser :VisualExpression list list parser = ws >>. pstring "[" >>. sepBy list_parser (pstring ",") .>> pstring "]" .>> eof

    let parse text : Result<VisualExpression, string> =
        match run parser text with
        | ParserResult.Success (result,_,_) -> Result.Ok result
        | ParserResult.Failure (error,_,_) -> Result.Error error

    let parse_list s : Result<VisualExpression list, string> =
        match run list_parser s with
        | ParserResult.Success (result,_,_) -> Result.Ok result
        | ParserResult.Failure (error,_,_) -> Result.Error error

    let parse_list_list s : Result<VisualExpression list list, string> =
        match run list_list_parser s with
        | ParserResult.Success (result,_,_) -> Result.Ok result
        | ParserResult.Failure (error,_,_) -> Result.Error error

module private InfixFormatter =

    open Operators

    let culture = System.Globalization.CultureInfo.InvariantCulture

    let private dropParenthesis = function
        | VisualExpression.Parenthesis x -> x
        | x -> x

    let rec format write = function
        | VisualExpression.Symbol s ->
            match s with
            | "pi" -> write "\u03C0" // "π"
            | x -> write x
        | VisualExpression.Infinity -> write "\u221E" // "∞"
        | VisualExpression.ComplexInfinity -> write "\u29DD" // "⧝"
        | VisualExpression.Undefined -> write "Undefined"
        | VisualExpression.ComplexI -> write "j"
        | VisualExpression.RealE -> write "e"
        | VisualExpression.RealPi -> write "\u03C0" // "π"
        | VisualExpression.PositiveInteger n -> write (n.ToString())
        | VisualExpression.PositiveFloatingPoint fp ->
            let s = fp.ToString(culture)
            if s.IndexOf('.') = -1 then write (s + ".0") else write s
        | VisualExpression.Parenthesis x ->
            write "("
            format write x
            write ")"
        | VisualExpression.Abs x ->
            write "|"
            format write x
            write "|"
        | VisualExpression.Negative x ->
            write "-"
            format write x
        | VisualExpression.Sum (x::xs) ->
            format write x
            xs |> List.iter (function
                | VisualExpression.Negative x -> write " - "; format write x
                | x -> write " + "; format write x)
        | VisualExpression.Product (x::xs) ->
            format write x
            xs |> List.iter (fun x -> write "*"; format write x)
        | VisualExpression.Fraction (n, d) ->
            format write n
            write "/"
            format write d
        | VisualExpression.Power (r, p) ->
            format write r
            write "^"
            format write p
        | VisualExpression.Root (r, p) when p = bigint 2 ->
            write "sqrt("
            format write (dropParenthesis r)
            write ")"
        | VisualExpression.Root (r, p) ->
            format write r
            write "^(1/"
            write (p.ToString())
            write ")"
        | VisualExpression.Function (fn, power, [x]) ->
            write fn
            if power.IsOne |> not then
                write "^"
                write (power.ToString())
            write "("
            format write x
            write ")"
        | VisualExpression.Function (fn, power, x::xs) ->
            write fn
            if power.IsOne |> not then
                write "^"
                write (power.ToString())
            write "("
            format write x
            xs |> List.iter (fun x -> write ","; format write x)
            write ")"
        | VisualExpression.Sum [] | VisualExpression.Product [] | VisualExpression.Function (_, _, []) -> failwith "invalid expression"

/// Print and parse infix expression string
[<RequireQualifiedAccess>]
module MathNetExprFormatter =

    open Microsoft.FSharp.Core

    let defaultStyle = { VisualExpressionStyle.CompactPowersOfFunctions = false }

    [<CompiledName("FormatVisual")>]
    let formatVisual visualExpression =
        let sb = StringBuilder()
        InfixFormatter.format (sb.Append >> ignore) visualExpression
        sb.ToString()

    /// Nicer human readable but slightly denormalized output
    [<CompiledName("FormatStyle")>]
    let formatStyle visualStyle expression =
        let sb = StringBuilder()
        let visual = VisualExpression.fromExpression visualStyle expression
        InfixFormatter.format (sb.Append >> ignore) visual
        sb.ToString()

    /// Nicer human readable but slightly denormalized output
    [<CompiledName("Format")>]
    let format expression = formatStyle defaultStyle expression

    [<CompiledName("FormatVisualWriter")>]
    let formatVisualWriter (writer:TextWriter) visualExpression =
        InfixFormatter.format (writer.Write) visualExpression

    /// Nicer human readable but slightly denormalized output
    [<CompiledName("FormatStyleWriter")>]
    let formatStyleWriter visualStyle (writer:TextWriter) expression =
        VisualExpression.fromExpression visualStyle expression |> formatVisualWriter writer

    /// Nicer human readable but slightly denormalized output
    [<CompiledName("FormatWriter")>]
    let formatWriter (writer:TextWriter) expression =
        VisualExpression.fromExpression defaultStyle expression |> formatVisualWriter writer

    [<CompiledName("ParseVisual")>]
    let parseVisual (infix: string) =
        MathNetExprParser.parse infix

    [<CompiledName("Parse")>]
    let parse (infix: string) =
        parseVisual infix |> Result.map VisualExpression.toExpression

    [<CompiledName("TryParse")>]
    let tryParse (infix: string) =
        match parse infix with
        | Ok expression -> Some expression
        | _ -> None

    [<CompiledName("ParseOrThrow")>]
    let parseOrThrow (infix: string) =
        match parse infix with
        | Ok expression -> expression
        | Error error -> failwith error

    [<CompiledName("ParseOrUndefined")>]
    let parseOrUndefined (infix: string) =
        match parse infix with
        | Ok expression -> expression
        | _ -> Expression.Undefined
