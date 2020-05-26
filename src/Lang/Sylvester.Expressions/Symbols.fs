namespace Sylvester

open System

[<AttributeUsage(AttributeTargets.All)>]
type SymbolAttribute(symbol:string) =
    inherit Attribute()
    member val Symbol = symbol

module Symbols =

    let Greek = Map.ofList [
        "epsilon", "fo"
    ] 
    