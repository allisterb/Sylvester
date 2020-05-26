namespace Sylvester

open System
open FSharp.Quotations

[<AttributeUsage(AttributeTargets.All)>]
type SymbolAttribute(symbol:string) =
    inherit Attribute()
    member val Symbol = symbol

module Symbols =

    let BuiltIn = Map.ofList [
       getExprName <@ not @>, "\u00AC"
    ]

    let Greek = Map.ofList [
        "epsilon", "fo"
    ] 
    