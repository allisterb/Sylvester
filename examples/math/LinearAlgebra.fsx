#load "Include.fsx"

open System
open Sylvester
open Patterns
open FSharp.Quotations

//getMethodInfo <@ Math.Sqrt @>
let x,y,z = var3<real>
let a = 2.
let ff = expand_list <@[a] @>
let ss = <@[ 
        [z; 7.; 8.] 
        [7.;8.;9.] 
        ]@> 
ss |> expand_list |> List.map expand_list        
Mat<three, two>(ss).Expr        //|> expand_list_values'<real>

replace_var_type<int> ss
let M = Mat<three, two> <@[[x; 7.; 8.]; [7.;8.;9.]]@>
M._Array

//<@[ [6.; 7.; 8.]; [7.;8.;9.] ]@> |> expand |> get_vars
//<@[ [x; y; 8.]; [7.;8.;9.] ]@>
//let Ar = Mat<three, two> <@[[6.; 7.; 8.]; [7.; 8.; 9.]]@> 
//let s = (Ar + Ar) //|> Matrix<int>.toDouble
//let re = s.toInt32() //|> Matrix<three, two, Rational> 
//r.
//Matrix<int>.toDouble s
//Ar //|>Mat<three, two, int>.toDouble

//Ar.Convert<float>()
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

