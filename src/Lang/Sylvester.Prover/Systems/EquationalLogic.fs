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
        | (S, A, B) when sequal (S A) B -> Some (S, (A, B))
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
            when vequal avar bvar && vequal avar cvar && vequal avar dvar && (S (Expr.Lambda(cvar, cbody), Expr.Lambda(dvar, dbody))) -> let (A,B) = Expr.Lambda(avar, abody), Expr.Lambda(bvar, bbody) in Some (S.ToString(), (A, B))
         | _ -> None

    (*  *)
  
