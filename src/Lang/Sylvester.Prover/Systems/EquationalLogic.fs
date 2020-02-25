namespace Sylph

open FSharp.Quotations
open FSharp.Quotations.ExprShape

open Sylvester

/// Formalizes the system of equational logic used by Sylph
/// Based on E: http://www.cs.cornell.edu/home/gries/Logic/Equational.html
/// The main difference is that since we only have to deal with symbolic equality (not mathematical equality)
//  we can drop the restriction that a substitution must only replace variables in an expression.
module EquationalLogic =
    (* Patterns *)

    /// Expr A is transformed by a substitution S into B.
    let (|Substitution|_|) =
        function
        | (S, A, B) when sequal (S A) B -> Some (S, A, B)
        | _ -> None

    /// A pair (Expr A, Expr B) is transformed by a substitution S into (C, D).
    let (|Substitution2|_|) =
        function
        | (S, A, B, C,D) when let (c, d) = S(A, B) in sequal2 c d C D -> Some (S, A, B, C, D)
        | _ -> None

    /// A formula is an expression like fun x -> 2 * x.
    let (|Formula|_|) =
        function
        | ShapeLambda(avar, abody) -> Expr.Lambda(avar, abody) |> Some
        | _ -> None

    /// A formula substitution from one formula to another of the same type.
    let (|FormulaSubstitution|_|) =
        function
        | Substitution(_, Formula(ShapeLambda(avar, abody)), Formula(ShapeLambda(bvar, bbody))) 
                                when vequal avar bvar -> Expr.Lambda(avar, abody) |> Some
        | _ -> None

    /// A theorem has the form Formula A == Formula B where the pair (A, B) is transformed by 
    //  substitution into (C, D)
    let (|Theorem|_|) =
        function
        | Substitution2(_, Formula(ShapeLambda(avar, abody)), Formula (ShapeLambda(bvar, bbody)), 
                        Formula (ShapeLambda(cvar, _)), Formula (ShapeLambda(dvar, _)))   
           when vequal avar bvar && vequal avar cvar && vequal avar dvar -> Some (Expr.Lambda(avar, abody), Expr.Lambda(bvar, bbody))
        | _ -> None

    (* Axioms *)

    /// A == A.
    let (|Reflexivity|_|) =
        function
        | Theorem(A, B), Theorem(C, D) 
             when sequal2 A B C D  -> Some true
        | _ -> None


    /// If A == B is a theorem then we can infer B == A.
    let (|Symmetry|_|) =
        function
        | Theorem(A, B), Theorem(C, D) 
             when sequal2 A B D C  -> Some true
        | _ -> None

    /// If A == B is a theorem and B == C is a theorem then we can infer A == C.
    let (|Transitivity|_|) =
        function
        | Theorem(body1, body2), Theorem(body3, body4), Theorem(body5, body6) 
             when sequal body1 body5 && sequal body2 body3 && sequal body4 body5 && sequal body2 body6  -> Some true
        | _ -> None
 
    /// If A == B is a theorem then we can infer S(A) == S(B).
    let (|Leibniz|_|) =
        function
        | (S, Theorem(labody, rabody), Theorem(lbbody, rbbody)) 
            when sequal2 lbbody rbbody (S labody) (S rabody) -> Some true
        | _ -> None

    //let E =
    //    function
    //    | Reflexivity x
    //    | Symmetry
    //   
    //    | Leibniz -> Some true
        | _ -> None
        
        