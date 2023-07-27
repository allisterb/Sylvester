namespace Sylvester

open System
open System.Collections.Generic
open FSharp.Quotations

[<AttributeUsage(AttributeTargets.All)>]
type SymbolAttribute(symbol:string) =
    inherit Attribute()
    member val Symbol = symbol

[<RequireQualifiedAccess>]
module LatinVars =
    let A = "A"
    let a = "a"
    let B = "B"
    let b = "b"
    let C =  "C"
    let c =  "c"
    let D =  "D"
    let d =  "d"
    let E =  "E"
    let e =  "e"
    let F =  "F"
    let f =  "f"
    let N =  "N"
    let n =  "n"
    let X =  "X"
    let x =  "x"
    let Y =  "Y"
    let y =  "y"
    let Z =  "Z"
    let z =  "z"

[<AutoOpen>]
module GreekVars =        
    let epsilon = "epsilon"
    let theta = "theta"
    
[<RequireQualifiedAccess>]
module LatinDiffs =
    let private var'<'t> n = let v = Expr.Var(Var(n, typeof<'t>)) in <@ %%v:'t @>
    let x<'t> = var'<'t> "dx"

module Symbols =
    let private src expr = Swensen.Unquote.Operators.decompile expr
    let BulitIn = 
        let b = new Dictionary<string, string>()
        b.Add(src <@ not @>, "\u00AC")
        b

    let GreekUnicode = Map.ofList [
        "epsilon", "\u03f5"
        "theta", "\u03b8"
    ]
    
    let GreekLatex = Map.ofList [
           "epsilon", "\\epsilon"
           "theta", "foo"
    ]
    let isGreek s = GreekUnicode.ContainsKey s

    let mutable TransliterateGreek = true
    

