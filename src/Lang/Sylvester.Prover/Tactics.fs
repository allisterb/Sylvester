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
    /// The constant true is a theorem
    let Truth commute rule = 
        let proof = match rule with | Derive(_,p,_) -> p | _ ->  failwith "This rule is not a derived rule."
        let l = 
            match proof.Stmt with 
            | Equals(l, r) when sequal r <@ true @> -> l
            | _ -> failwith "This theorem is not an identity with the constant true."
        let theory = proof.Theory
        let true_id = ident theory <@(true = true) = true@> [LR commute]
      
        let stmt = <@@ (%%l:bool) = (true = true) @@>
        let p = Proof(stmt, proof.Theory, R true_id :: proof.Steps, true) in 
                Theorem(stmt, p) |> Ident

    /// If A = B is a theorem then so is B = A.
    let Trn commute rule =
        let proof = match rule with | Derive(_,p,_) -> p | _ ->  failwith "This rule is not a derived rule."
        let stmt = 
            match proof.Stmt with 
            | Equals(l, r) -> <@@ (%%r:bool) = (%%l:bool) @@>
            | _ -> failwith "This theorem is not an identity."
        let p = Proof(stmt, proof.Theory, LR commute :: proof.Steps, true) in 
            Theorem(stmt, p) |> Ident

    /// If (L = R) = X is a theorem then so is (R = L) = X.
    let TrnL commute rule =
        let proof = match rule with | Derive(_,p,_) -> p | _ ->  failwith "This rule is not a derived rule."
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
        let proof = match rule with | Derive(_,p,_) -> p | _ ->  failwith "This rule is not a derived rule."
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

    /// If (A1 = A2) = A3 is a theorem then so is A1 = (A2 = A3)
    let RightAssoc' lassoc rule =
         let proof = match rule with | Derive(_,p,_) -> p | _ ->  failwith "This rule is not a derived rule."
         let stmt = 
             match proof.Stmt with 
             | Equals(Equals(l1, l2), r) -> <@@ (%%l1:bool) = ((%%l2:bool) = (%%r:bool)) @@>
             | _ -> failwith "This theorem is not an identity."
         let p = Proof(stmt, proof.Theory, LR lassoc :: proof.Steps, true) in 
             Theorem(stmt, p) |> Ident

    /// If A1 = (A2 =  A3) is a theorem then so is (A1 = A2) = A3
    let LeftAssoc' rassoc rule =
         let proof = match rule with | Derive(_,p,_) -> p | _ ->  failwith "This rule is not a derived rule."
         let stmt = 
             match proof.Stmt with 
             | Equals(l, Equals(r1, r2)) -> <@@ ((%%l:bool) = (%%r1:bool)) = (%%r2:bool) @@>
             | _ -> failwith "This theorem is not an identity."
         let p = Proof(stmt, proof.Theory, LR rassoc :: proof.Steps, true) in 
             Theorem(stmt, p) |> Ident
    /// If A = B is a theorem then so is (A = B) = true.
    let Taut ident rule =
        let proof = match rule with | Derive(_,p,_) -> p | _ ->  failwith "This rule is not a derived."
        let theory = proof.Theory
        let expr = proof.Stmt
        let stmt = <@@ (%%expr) = true @@>
        let p = Proof(stmt, theory, (expr |> ident |> LR) :: proof.Steps, true) in 
            Theorem(stmt, p) |> Ident 
