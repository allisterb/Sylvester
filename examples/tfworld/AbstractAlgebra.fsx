#load "MathInclude.fsx"

open System 
open System.Linq

open Sylvester

// Define an infinte seq
let c = infiniteSeq ((+) 65 >> Char.ConvertFromUtf32) 


c.Take(26) |> Array.ofSeq
let cats = Monoid(c, (+), "")

cats.Take 3 |> Array.ofSeq

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