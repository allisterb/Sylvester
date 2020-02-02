#load "MathInclude.fsx"

open System 
open System.Linq

open Microsoft.FSharp.Quotations
open System.Runtime.CompilerServices

open Sylvester

Z5.Set
// Define an infinte seq
//let c = infiniteSeq ((+) 65 >> Char.ConvertFromUtf32) 


//c.Take(26) |> Array.ofSeq
//let cats = Monoid(c, (+), "")

//cats.Take 3 |> Array.ofSeq

//let a = "Nancy"
//let b = "Drew"
//alpha.Op a b
//let alpa = alpha.Op
//"nancy" ++ "drew"
//let prefix  = (+) "prefix-"  
//let a' = prefix a
//let b' = prefix b
//let h = Groupoids(alpha, alpha, prefix)
//h.[a']

//(alpha.["nancy", "drew"]) 
//let h = Groupoids(alpha, alpha, prefix)
//alpha.["nancy", "drew"]


//let n = d.["nancy", "drew"]
//let o = d.["herman", "melville"]
//h.[n, o]
//d.[h.[n], h.[o]] 
//let m = Morph(d,d,id)

//let g = Seq ["A"; "A"; "B"]

//g.Take(30) |> Array.ofSeq

//Integers.Group.Take(10) |> Array.ofSeq

[<ReflectedDefinition>]
let f x = 
    let a = 12. * 4. - float x
    a ** 2.

//let g = FsExpr(f)
///g.Expr
let x = Seq (seq { yield 1})
let z  = match x with | Seq s -> s |_ -> failwith ""
//typeof<FsExpr<_> list>.GetG
 
let o = Poset(Z, (<))
o.[4, 2]