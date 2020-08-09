#load "Include.fsx"

open System
open Sylvester
open Patterns
open FSharp.Quotations

//getMethodInfo <@ Math.Sqrt @>
let x,y,z = var3<float>


let Ar = Matrix<two, three, int> <@[ [4; 5; 6]; [7;8;9] ]@> 
Ar.Convert<float>()
//Ar._Matrix.Value.RowCount
//let ar = Ar :> IPartialShape<two>

//ar.Dims.Value.[0]
//Ar.Expr |> Option.get |> src

//expand_values<int> Ar
//Ar |> expand_list |> List.map (fun l -> match l with | List ea -> if ea |>List.forall (fun )   | _ -> failwith "foo")
//match Ar with
//| List (e) -> e
//| _ -> failwith "Not a list"


//let ent = <@ 3. * x + 2. * y + 6. * x + z @> 




//|> algebra_simplify |> polyn_coeffs <@ x @>
(*
let ggg = <@[
    3 * x + 5 = 0Q
    4 * x - 5 = 0Q
    9 * x + 1 = 0Q
]@>
*)

