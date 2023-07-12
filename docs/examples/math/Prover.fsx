#nowarn "3391"
#load "Include.fsx"

open Sylvester
open Patterns
open FSharp.Quotations
open PropCalculus


let ident (theory:Theory) (e:Prop) steps =
       let f = e.Expr |> expand in
       match f with
       | Equals(_, _) -> Theorem(f, Proof (f, theory, steps, true)) |> Ident
       | _ -> failwithf "The expression %s is not an identity." (theory.PrintFormula f)
   
let logical_ident steps (e:Prop) = ident Proof.Logic e steps
   
let id_ax theory (e:Prop) = ident theory e []
   
let log_id_ax (e:Prop) = id_ax Proof.Logic e

let def_true (p:Prop) = id_ax prop_calculus (true == (p == p))  

proof prop_calculus (-F == T).Expr [
    LR commute
    def_true F  |> L
    LR right_assoc
    apply_right commute
    apply_right collect
    def_true F |> Commute |> apply_right  
]
