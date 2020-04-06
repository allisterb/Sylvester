namespace Sylvester

open Patterns

module Tactics = 
    /// Switch the LHS of an identity with the RHS.
    let Transpose commute rule =
        let proof = match rule with | Rule.Subst(_,p,_) -> p | _ ->  failwith "This rule is not a substitution."
        let (l, r) = 
            match proof.Stmt with 
            | BinaryCall(l, r) -> (l, r)
            | _ -> failwith "This theorem is not an identity."
        let p = Proof(<@@ (%%r:bool) = (%%l:bool) @@>, proof.Theory, LR commute :: proof.Steps, true) in 
            Theorem(<@@ (%%r:bool) = (%%l:bool) @@>, p) |> Ident