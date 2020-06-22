namespace Sylvester

open System.IO
open System.Reflection
open System.Text

open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns
open FSharp.Quotations.ExprShape
open FSharp.Reflection

open FParsec

open FsExpr
    
module ExprParser =
    
    type ParseResult =
        | ParsedExpression of Expr
        | ParseFailure of string

    type 'a parser = Parser<'a, unit>

    module private Ops =
        let (|&|) (l:bool) (r:bool) = l && r 
    open Ops

    let ws = spaces
    let str_ws s = pstring s .>> ws
    let parens p = between (str_ws "(") (str_ws ")") p
    //let abs p = between (str_ws "|") (str_ws "|") p |>> Expr.Call()

    let identifier : Expr parser =
        let isMathChar = function | '\u03C0' | '\u221E' | '\u29DD' -> true | _ -> false
        let isIdentifierFirstChar c = isLetter c || isMathChar c
        let isIdentifierChar c = isLetter c || isDigit c || isMathChar c || c = '_'
        many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier" .>> ws
        |>> function 
            | "true" -> Expr.Value true
            | "false" -> Expr.Value false
            | id -> Expr.Var(Var(id, typeof<bool>))

    let expression : Expr parser =
        let opp = OperatorPrecedenceParser<Expr,unit,unit>()
        let expr = opp.ExpressionParser

        let parensTerm = parens expr

        let term = parensTerm <|> identifier

        let and' l r =  call <@ (|&|) @> (l::r::[])
        //let or' l r = call <@ (|||) @> (l::r::[])
        //let implies' l r = call <@ (==>)@> (l::r::[])
        opp.TermParser <- term
        opp.AddOperator(InfixOperator("+", ws, 1, Associativity.Left, and'))
        expr

    let parser : Expr parser = ws >>. expression .>> eof

    let parse text =
        match run parser text with
        | ParserResult.Success (result,_,_) -> ParsedExpression result
        | ParserResult.Failure (error,_,_) -> ParseFailure error

    


