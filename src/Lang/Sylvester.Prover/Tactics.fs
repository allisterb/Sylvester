namespace Sylvester

open FSharp.Quotations
open Patterns

[<AutoOpen>]
module ProofOps =
    let last_state (p:Proof) = p.LastState

    let left_state p = p |> last_state |> expand_left

    let right_state p = p |> last_state |> expand_right

    let left_src p = p |> left_state |> src

    let right_src p = p |> right_state |> src

module Tactics = 
    /// If A = B is a theorem then so is B = A.
    let Trn commute rule =
        let proof = match rule with | Rule.Subst(_,p,_) -> p | _ ->  failwith "This rule is not a substitution."
        let stmt = 
            match proof.Stmt with 
            | Equals(l, r) -> <@@ (%%r:bool) = (%%l:bool) @@>
            | _ -> failwith "This theorem is not an identity."
        let p = Proof(stmt, proof.Theory, LR commute :: proof.Steps, true) in 
            Theorem(stmt, p) |> Ident

    /// If (L = R) = X is a theorem then so is (R = L) = X.
    let TrnL commute rule =
        let proof = match rule with | Rule.Subst(_,p,_) -> p | _ ->  failwith "This rule is not a substitution."
        let (l, r) = 
            match proof.Stmt with 
            | Equals(l, r) -> (l, r)
            | _ -> failwith "This theorem is not an identity."
        let l1 = 
            match l with 
            | Patterns.Call(o, m, l::r::[]) -> binary_call(o, m, r, l)
            | _ -> failwith "The LHS of this theorem is not an identity."

        let stmt = <@@ ((%%l1:bool)) = (%%r:bool) @@>
        let p = Proof(stmt, proof.Theory, L commute :: proof.Steps, true) in 
            Theorem(stmt, p) |> Ident

    /// If X = (L = R) is a theorem then so is X = (R = L).
    let TrnR commute rule =
        let proof = match rule with | Rule.Subst(_,p,_) -> p | _ ->  failwith "This rule is not a substitution."
        let (l, r) = 
            match proof.Stmt with 
            | Equals(l, r) -> (l, r)
            | _ -> failwith "This theorem is not an identity."
        let r1 = 
            match r with 
            | Patterns.Call(o, m, l::r::[]) -> binary_call(o, m, r, l)
            | _ -> failwith "The LHS of this theorem is not an identity."

        let stmt = <@@ ((%%l:bool)) = (%%r1:bool) @@>
        let p = Proof(stmt, proof.Theory, L commute :: proof.Steps, true) in 
            Theorem(stmt, p) |> Ident

    /// If A = B is a theorem then so is (A = B) = true.
    let Taut ident rule =
        let proof = match rule with | Rule.Subst(_,p,_) -> p | _ ->  failwith "This rule is not a substitution."
        let expr = proof.Stmt
        let stmt = <@@ (%%expr) = true @@>
        let p = Proof(stmt, proof.Theory, (expr |> ident |> LR) :: proof.Steps, true) in 
            Theorem(stmt, p) |> Ident
