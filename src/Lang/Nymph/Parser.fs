namespace Sylvester.Nymph

open System.IO
open System.Reflection
open System.Text

open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns
open FSharp.Quotations.ExprShape
open FSharp.Reflection

open FParsec

open Sylvester.FsExpr
    

module Parser =
    type RuntimeResult<'s, 'f> = Sylvester.FsRuntime.Result<'s, 'f>
    type ParseResult =
        | ParsedExpression of Expr
        | ParseFailure of string
    type 'a parser = Parser<'a, unit>
 
    let ws = spaces
    let str_ws s = pstring s .>> ws
    let parens p = between (str_ws "(") (str_ws ")") p
    let isMathChar = function | '\u03C0' | '\u221E' | '\u29DD' -> true | _ -> false
    let isIdentifierFirstChar c = isLetter c || isMathChar c
    let isIdentifierChar c = isLetter c || isDigit c || isMathChar c || c = '_'

    let boolExprParser : Expr parser =
        let boolIdentifier : Expr parser =
            many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier" .>> ws 
            |>> 
            function 
            | "true" -> Expr.Value true
            | "false" -> Expr.Value false
            | id -> Expr.Var(Var(id, typeof<bool>))

        let opp = OperatorPrecedenceParser<Expr,unit,unit>()
        let expr = opp.ExpressionParser
        let parensTerm = parens expr
        let term = parensTerm <|> boolIdentifier

        let _equal l r =  call <@ (=) @> (l::r::[])
        let _not l =  call <@ Sylvester.Formula.(|&|) @> (l::[])
        let _implies l r = call <@ Sylvester.Formula.(==>) @> (l::r::[])
        let _conseq l r = call <@ Sylvester.Formula.(<==) @> (l::r::[])
        let _and l r =  call <@ Sylvester.Formula.(|&|) @> (l::r::[])
        let _or l r = call <@ Sylvester.Formula.(|||) @> (l::r::[])
      
        opp.TermParser <- term
        opp.AddOperator(InfixOperator("=", ws, 1, Associativity.Left, _equal))
        opp.AddOperator(PrefixOperator("not", ws, 3, true, _not))
        opp.AddOperator(InfixOperator("implies", ws, 2, Associativity.Left, _implies))
        opp.AddOperator(InfixOperator("conseq", ws, 2, Associativity.Left, _conseq))
        opp.AddOperator(InfixOperator("and", ws, 2, Associativity.Left, _and))
        opp.AddOperator(InfixOperator("or", ws, 2, Associativity.Left, _or))
        ws >>. expr .>> eof

    let intExprParser : Expr parser =
        let number = 
            let options =
                NumberLiteralOptions.AllowFraction
                ||| NumberLiteralOptions.AllowFractionWOIntegerPart
                ||| NumberLiteralOptions.AllowInfinity
                ||| NumberLiteralOptions.AllowExponent
            numberLiteral options "number" .>> ws|>> fun num -> Expr.Value(float num.String)

        let integerIdentifier : Expr parser = many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier" .>> ws |>> (fun id -> Expr.Var(Var(id, typeof<int>)))
        let opp = OperatorPrecedenceParser<Expr,unit,unit>()
        let expr = opp.ExpressionParser
        let parensTerm = parens expr
        let term = parensTerm <|> number <|> integerIdentifier

        let _equal l r =  call <@ (=) @> (l::r::[])
        let _implies l r = call <@ Sylvester.Formula.(==>) @> (l::r::[])
        let _not l =  call <@ Sylvester.Formula.(|&|) @> (l::[])  
        let _and l r =  call <@ Sylvester.Formula.(|&|) @> (l::r::[])
        let _or l r = call <@ Sylvester.Formula.(|||) @> (l::r::[])
      
        opp.TermParser <- term
        opp.AddOperator(InfixOperator("=", ws, 1, Associativity.Left, _equal))
        opp.AddOperator(PrefixOperator("implies", ws, 3, true, _not))
        opp.AddOperator(PrefixOperator("not", ws, 3, true, _not))
        opp.AddOperator(InfixOperator("and", ws, 2, Associativity.Left, _and))
        opp.AddOperator(InfixOperator("or", ws, 2, Associativity.Left, _or))
        ws >>. expr .>> eof

    let parse<'t> text =
        let parser = 
            match typeof<'t>.Name with
            | "Boolean" -> boolExprParser |> RuntimeResult.Success
            | t -> sprintf "Cannot parse expression of type %s." t |> exn |> RuntimeResult.Failure
        match parser with
        | RuntimeResult.Success p ->
            match run p text with
            | ParserResult.Success (result,_,_) -> result |> RuntimeResult.Success
            | ParserResult.Failure (error,_,_) -> sprintf "Failed to parse the expression %A as an F# expression." error |> exn |> RuntimeResult.Failure
        | RuntimeResult.Failure f -> RuntimeResult.Failure f