namespace Sylvester

open FSharp.Quotations
open Patterns

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

    /// If A is a theorem then so is A = true
    let Taut ident (t:Theorem) =
        let proof = t.Proof
        let theory = proof.Theory
        let expr = proof.Stmt
        let stmt = <@@ (%%expr) = true @@>
        let p = Proof(stmt, theory, (expr |> ident |> LR) :: proof.Steps, true) in 
        Theorem(stmt, p) |> Ident 

    /// If A = B is a theorem then so is (A = B) = true.
    let Taut' ident (rule:Rule) =
        let proof = match rule with | Derive(_,p,_) -> p | _ ->  failwith "This rule is not a derived rule."
        let theory = proof.Theory
        let expr = proof.Stmt
        let stmt = <@@ (%%expr) = true @@>
        let p = Proof(stmt, theory, (expr |> ident |> LR) :: proof.Steps, true) in 
        Theorem(stmt, p) |> Ident 

    /// If A = B is a theorem then so is B = A.
    let Commute commute rule =
        let proof = match rule with | Derive(_,p,_) -> p | _ ->  failwith "This rule is not a derived rule."
        let stmt = 
            match proof.Stmt with 
            | Equals(l, r) -> <@@ (%%r:bool) = (%%l:bool) @@>
            | _ -> failwith "This theorem is not an identity."
        let p = Proof(stmt, proof.Theory, LR commute :: proof.Steps, true) in 
        Theorem(stmt, p) |> Ident

    /// If (L = R) = X is a theorem then so is (R = L) = X.
    let CommuteL commute rule =
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
    let CommuteR commute rule =
        let proof = match rule with | Derive(_,p,_) -> p | _ ->  failwith "This rule is not a derived rule."
        let (l, r) = 
            match proof.Stmt with 
            | Equals(l, r) -> (l, r)
            | _ -> failwith "This theorem is not an identity."
        let r1 = 
            match r with 
            | Patterns.Call(o, m, l::r::[]) -> binary_call(o, m, r, l)
            | _ -> failwith "The rHS of this theorem is not an identity."

        let stmt = <@@ ((%%l:bool)) = (%%r1:bool) @@>
        let p = Proof(stmt, proof.Theory, R commute :: proof.Steps, true) in 
        Theorem(stmt, p) |> Ident

    /// If (A1 = A2) = A3 is a theorem then so is A1 = (A2 = A3)
    let RightAssoc lassoc rule =
         let proof = match rule with | Derive(_,p,_) -> p | _ ->  failwith "This rule is not a derived rule."
         let stmt = 
             match proof.Stmt with 
             | Equals(Equals(l1, l2), r) -> <@@ (%%l1:bool) = ((%%l2:bool) = (%%r:bool)) @@>
             | _ -> failwith "This theorem is not an identity."
         let p = Proof(stmt, proof.Theory, LR lassoc :: proof.Steps, true) in 
         Theorem(stmt, p) |> Ident

    /// If A1 = (A2 =  A3) is a theorem then so is (A1 = A2) = A3
    let LeftAssoc rassoc rule =
         let proof = match rule with | Derive(_,p,_) -> p | _ ->  failwith "This rule is not a derived rule."
         let stmt = 
             match proof.Stmt with 
             | Equals(l, Equals(r1, r2)) -> <@@ ((%%l:bool) = (%%r1:bool)) = (%%r2:bool) @@>
             | _ -> failwith "This theorem is not an identity."
         let p = Proof(stmt, proof.Theory, LR rassoc :: proof.Steps, true) in 
         Theorem(stmt, p) |> Ident

    let RightAssocL lassoc rule =
         let proof = match rule with | Derive(_,p,_) -> p | _ ->  failwith "This rule is not a derived rule."
         let (l, r) = 
             match proof.Stmt with 
             | Equals(l, r) -> (l, r)
             | _ -> failwith "This theorem is not an identity."
         let stmt = 
             match l with 
             | Equals(Equals(l1, l2), r2) -> <@@ ((%%l1:bool) = ((%%l2:bool) = (%%r2:bool))) = (%%r:bool) @@>
             | _ -> failwith "This theorem is not an identity."
         let p = Proof(stmt, proof.Theory, L lassoc :: proof.Steps, true) in 
         Theorem(stmt, p) |> Ident

    let RightAssocR lassoc rule =
         let proof = match rule with | Derive(_,p,_) -> p | _ ->  failwith "This rule is not a derived rule."
         let (l, r) = 
             match proof.Stmt with 
             | Equals(l, r) -> (l, r)
             | _ -> failwith "This theorem is not an identity."
         let stmt = 
             match r with 
             | Equals(Equals(l1, l2), r2) -> <@@ (%%l:bool) = ((%%l1:bool) = ((%%l2:bool) = (%%r2:bool)))  @@>
             | _ -> failwith "This theorem is not an identity."
         let p = Proof(stmt, proof.Theory, R lassoc :: proof.Steps, true) in 
         Theorem(stmt, p) |> Ident

    let LeftAssocL rassoc rule =
         let proof = match rule with | Derive(_,p,_) -> p | _ ->  failwith "This rule is not a derived rule."
         let (l, r) = 
             match proof.Stmt with 
             | Equals(l, r) -> (l, r)
             | _ -> failwith "This theorem is not an identity."
         let stmt = 
             match l with 
             | Equals(l1, Equals(r1, r2)) -> <@@ (((%%l1:bool) = (%%r1:bool)) = (%%r2:bool)) = (%%r:bool) @@>
             | _ -> failwith "The LHS of this theorem is not an identity."
         let p = Proof(stmt, proof.Theory, L rassoc :: proof.Steps, true) in 
         Theorem(stmt, p) |> Ident

    let LeftAssocR rassoc rule =
         let proof = match rule with | Derive(_,p,_) -> p | _ ->  failwith "This rule is not a derived rule."
         let (l, r) = 
             match proof.Stmt with 
             | Equals(l, r) -> (l, r)
             | _ -> failwith "This theorem is not an identity."
         let stmt = 
             match r with 
             | Equals(l1, Equals(r1, r2)) -> <@@ (%%l:bool) = (((%%l1:bool) = (%%r1:bool)) = (%%r2:bool)) @@>
             | _ -> failwith "The RHS of this theorem is not an identity."
         let p = Proof(stmt, proof.Theory, R rassoc :: proof.Steps, true) in 
         Theorem(stmt, p) |> Ident

    let MutualImplication theory taut ident stmt =
        let (l, r) = 
            match stmt with 
            | Equals(l, r) -> (l, r)
            | _ -> failwith "This statement is not an identity."

        let lhs steps =
            let s = <@@ %%l ==> %%r @@>
            let p = Proof(s, theory, steps, true) in
            Theorem(s, p)

        let rhs steps =
            let s = <@@ %%r ==> %%l @@>
            let p = Proof(s, theory, steps, true) in
            Theorem(s, p)

        let p lhs rhs = proof theory stmt [
            ident |> LR
            lhs |> taut |> L
            rhs |> taut |> R
        ]

        lhs, rhs, p