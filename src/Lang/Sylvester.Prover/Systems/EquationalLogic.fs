namespace Sylph

open FSharp.Quotations
open FSharp.Quotations.ExprShape

open Sylvester

/// Formalizes the system of equational logic (E) used by Sylph
/// http://www.cs.cornell.edu/home/gries/Logic/Equational.html
module EquationalLogic =
    (* Patterns *)

    /// Expr A is transformed by a recursive substitution S into B.
    let (|Substitution|_|) =
        function
        | (S, A, B) when sequal B (traverse A S) -> Some (A, B)
        | _ -> None

    /// A formula is an expression like fun x -> 2 * x.
    //let (|Formula|_|) =
    //    function
    //    | ShapeLambda(avar, abody) -> Some (avar, abody)
    //    | _ -> None

    /// A theorem has the form Formula A == Formula B where the pair (A, B .
    //let (|Theorem|_|) =
    //    function
    //    | Formula(avar, abody), Formula(bvar, bbody) 
    //        when vequal avar bvar && sequal bbody (traverse abody S) -> Some (S, abody, bbody)
    //    | _ -> None

    (* Axioms *)

    /// A == A.
    //let (|Reflexivity|_|) =
    //    function
    //    | Theorem(S1, body1, body2), Theorem(_, body3, body4) 
    //         when sequal2 body1 body2 body3 body4  -> Some true
    //    | _ -> None


    /// If A == B is a theorem then we can infer B == A.
    ///let (|Symmetry|_|) =
    ///    function
    //    | Theorem(body1, body2), Theorem(body3, body4) 
    ///         when sequal2 body1 body2 body4 body3  -> Some true
    //    | _ -> None

    /// If A == B is a theorem and B == C is a theorem then we can infer A == C.
    //let (|Transitivity|_|) =
    //    function
    //    | Theorem(body1, body2), Theorem(body3, body4), Theorem(body5, body6) 
    //         when sequal body1 body5 && sequal body2 body3 && sequal body4 body5 && sequal body2 body6  -> Some true
    //    | _ -> None
 
    /// If A == B is a theorem then we can infer S(A) == S(B).
    //let (|Leibniz|_|) =
    //    function
    //    | (S, Theorem(labody, rabody), Theorem(lbbody, rbbody)) 
    //        when sequal2 lbbody rbbody (traverse labody S) (traverse rabody S) -> Some true
    //    | _ -> None

    //let E =
    //    function
    //    | Reflexivity x
    //    | Symmetry
    //   
    //    | Leibniz -> Some true
        | _ -> None
        
        