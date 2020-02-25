namespace Sylph

open FSharp.Quotations
open FSharp.Quotations.ExprShape

open Sylvester

/// Formalizes the system of equational logic used by Sylph
/// Based on http://www.cs.cornell.edu/home/gries/Logic/Equational.html
/// The main difference is that since we only have to deal with symbolic equality (not mathematical equality)
//  we can drop the restriction that a substitution must only replace variables in an expression.
module EquationalLogic =
    
    (* Patterns *)

    /// Expr A is transformed by a substitution S into B.
    let (|Substitution|_|) =
        function
        | (S, A, B) when sequal (S A) B -> Some (S, A, B)
        | _ -> None

    /// A formula is an expression like fun x -> 2 * x.
    let (|Formula|_|) =
        function
        | ShapeLambda(avar, abody) -> Expr.Lambda(avar, abody) |> Some
        | _ -> None

    /// A pair (Expr A, Expr B) is transformed by a substitution S into (C, D).
    let (|Substitution2|_|) =
        function
        | (S, (A, B), (C,D)) when let (c, d) = S(A, B) in sequal2 c d C D -> Some (S, (A, B), (C, D))
        | _ -> None
    
    /// A theorem has the form Formula A == Formula B where the pair (A, B) is transformed by 
    ///  substitution into (C, D) such that in some system S, S |- (C == D)
    let (|Theorem|_|) =
        function
         | S, Substitution2(_, (Formula(ShapeLambda(avar, abody)), Formula (ShapeLambda(bvar, bbody))), 
                         (Formula (ShapeLambda(cvar, cbody)), Formula (ShapeLambda(dvar, dbody))))
            when vequal avar bvar && vequal avar cvar && vequal avar dvar && (S (Expr.Lambda(cvar, cbody), Expr.Lambda(dvar, dbody))) -> let (A,B) = Expr.Lambda(avar, abody), Expr.Lambda(bvar, bbody) in Some (S.ToString(), A, B)
         | _ -> None

    (* Axioms *)

    /// A = A.
    let (|Reflexivity|_|) =
        function
        |  Theorem(S1, A1, B1), Theorem(S2, A2, B2) when S1 = S2 && sequal2 A1 A2 B1 B2 -> Some true 
        | _ -> None

    /// A == B = B == A.
    let (|Symmetry|_|) =
        function
        |  Theorem(S1, A1, B1), Theorem(S2, A2, B2) when S1 = S2 && sequal2 A2 B2 B1 A1 -> Some true
        | _ -> None

    /// If A == B  and B == C is a theorem then we can infer A == C.
    let (|Transitivity|_|) =
        function
        | Theorem(S1, A1, B1), Theorem(S2, A2, B2), Theorem(S3, A3, B3) when S1 = S2 -> Some true // TODO
        | _ -> None
 
    /// If A == B is a theorem then we can infer S(A) == S(B).
    let (|Leibniz|_|) =
        function
        | (S, Theorem(S1, A1, B1), Theorem(S2, A2, B2)) 
            when S1 = S2 && sequal2 A1 B1 (S A1) (S B1) -> Some true
        | _ -> None

            
    let rec subst (p:Proof) = 
        function
        | A when (sequal (p.A) (A)) && p.Complete -> p.B  
        | expr -> traverse expr (subst p)
    
    /// Substitute A with X when A == X.
    let Subst (p:Proof) = Subst(sprintf "Substitute %s in A with %s" (src p.A) (src p.B), p, fun proof e -> subst proof e) 
