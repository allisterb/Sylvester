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
    
[<RequireQualifiedAccess>]
module GreekVars =
    let private var'<'t> n = let v = Expr.Var(Var(n, typeof<'t>)) in <@ %%v:'t @>
    
    let epsilon<'t> = var'<'t> "\u03f5"