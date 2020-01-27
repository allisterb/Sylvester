#load "MathInclude.fsx"

open System 
open System.Linq

open Sylvester

[<RequireQualifiedAccess>] 
[<ReflectedDefinition>]
module op =
    let c = infiniteSeq2 ((+) 65 >> Char.ConvertFromUtf32)

    let e = (+) 65 >> Char.ConvertFromUtf32
let expr = FsExpr(op.e).Expr

//("D","D") |<| op.c 
 

//c.Take(26) |> Array.ofSeq

//let d = Monoid(c, (+), "")

//let m = Morph(d,d,id)

//let g = Seq ["A"; "A"; "B"]

//g.Take(30) |> Array.ofSeq

