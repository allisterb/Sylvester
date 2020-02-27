#load "Include.fsx"

open Sylph
open Formula
//let A = F (fun (p:bool) -> p = p)

//let B = F(fun (q:bool) -> q = q)

let f (p:bool) q r = (p = q = r)
let C = F (fun (p:bool) q r -> (p = q = r))

C.Expr
//C == Prop.T 

//open BooleanAlgebra
//let p1 = proof (C == Prop.T) boolean_algebra [
//    RightAssoc |> LeftA
//]