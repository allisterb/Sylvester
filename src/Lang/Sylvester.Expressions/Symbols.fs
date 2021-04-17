namespace Sylvester

open System
open System.Collections.Generic
open FSharp.Quotations

[<AttributeUsage(AttributeTargets.All)>]
type SymbolAttribute(symbol:string) =
    inherit Attribute()
    member val Symbol = symbol

module Symbols =
    let private src expr = Swensen.Unquote.Operators.decompile expr
    let BulitIn = 
        let b = new Dictionary<string, string>()
        b.Add(src <@ not @>, "\u00AC")
        b

    let Greek = Map.ofList [
        "epsilon", "fo"
    ] 
    
[<RequireQualifiedAccess>]
module GreekSymbols = 
    [<Literal>] 
    let epsilon = "\u03f5"

[<RequireQualifiedAccess>]
module GreekVars =
    let private var'<'t> n = let v = Expr.Var(Var(n, typeof<'t>)) in <@ %%v:'t @>
    
    let epsilon<'t> = var'<'t> GreekSymbols.epsilon

[<RequireQualifiedAccess>]
module LatinVars =
    let private var'<'t> n = let v = Expr.Var(Var(n, typeof<'t>)) in <@ %%v:'t @>
    let A<'t> = var'<'t> "A"
    let a<'t> = var'<'t> "a"
    let B<'t> = var'<'t> "B"
    let b<'t> = var'<'t> "b"
    let C<'t> = var'<'t> "C"
    let c<'t> = var'<'t> "c"
    let D<'t> = var'<'t> "D"
    let d<'t> = var'<'t> "d"
    let N<'t> = var'<'t> "N"
    let n<'t> = var'<'t> "n"