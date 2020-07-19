namespace Sylvester

open System
open System.Collections.Generic
open FSharp.Quotations

[<AttributeUsage(AttributeTargets.All)>]
type SymbolAttribute(symbol:string) =
    inherit Attribute()
    member val Symbol = symbol

module Symbols =

    let BulitIn = 
        let b = new Dictionary<string, string>()
        b.Add(src <@ not @>, "\u00AC")
        b

    let Greek = Map.ofList [
        "epsilon", "fo"
    ] 
    