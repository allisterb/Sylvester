namespace Sylvester

open FSharp.Quotations.DerivedPatterns
open Patterns

[<AutoOpen>]
module ProofOps =
    let last_state (p:Proof) = p.LastState

    let left_state p = p |> last_state |> expand_left

    let right_state p = p |> last_state |> expand_right

    let left_src p = p |> left_state |> src

    let right_src p = p |> right_state |> src

module Tactics = 
    /// Switch the LHS of an identity with the RHS.
    let Trn commute rule =
        let proof = match rule with | Rule.Subst(_,p,_) -> p | _ ->  failwith "This rule is not a substitution."
        let (l, r) = 
            match proof.Stmt with 
            | BinaryCall(l, r) -> (l, r)
            | _ -> failwith "This theorem is not an identity."
        let stmt = <@@ (%%r:bool) = (%%l:bool) @@>
        let p = Proof(stmt, proof.Theory, LR commute :: proof.Steps, true) in 
            Theorem(stmt, p) |> Ident

    /// (p = p) = true.
    let Taut ident rule =
        let proof = match rule with | Rule.Subst(_,p,_) -> p | _ ->  failwith "This rule is not a substitution."
        let (l, r) = 
            match proof.Stmt with 
            | BinaryCall(l, r) -> (l, r)
            | _ -> failwith "This theorem is not an identity."
        let expr = proof.Stmt
        let stmt = <@@ (%%expr) = true @@>
        let p = Proof(stmt, proof.Theory, (expr |> ident |> LR) :: proof.Steps, true) in 
            Theorem(stmt, p) |> Ident
