namespace Sylph

open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns

open Sylvester
open FormulaPatterns

/// Formalizes the theory of boolean algebra of a set closed under 2 binary operations that are associative, commutative, and idempotent,
/// 0 and 1 elements, and a complement operations.
module BooleanAlgebra =
    
    (* Patterns *)
  
    /// Join operation.
    let (|Join|_|) (op:'t->'t->'t) =
        function
        | SpecificCall <@@ op @@> (None,_,l::r::[]) -> Some (l,r)
        | _ -> None

    /// Meet operation.
    let (|Meet|_|) (op:'t->'t->'t) =
        function
        | SpecificCall <@@ op @@> (None,_,l::r::[]) -> Some (l,r)
        | _ -> None

    /// Complement operation.
    let (|Comp|_|) (op:'t->'t) =
        function
        | SpecificCall <@@ op @@> (None,_,r::[]) -> Some (r)
        | _ -> None

    /// Zero element.
    let (|Zero|_|) (m:'t) =
        function
        | Value (x, t) when t = typeof<'t> && m.Equals(x) -> Some x
        | _ -> None

    /// One element.
    let (|One|_|) (m:'t) =
        function
        | Value (x, t) when t = typeof<'t> && m.Equals(x) -> Some x
        | _ -> None

    (* Axioms *)

    /// Associativity axioms.
    let (|Assoc|_|) (joinOp: 't->'t->'t) (meetOp: 't->'t->'t) (desc: AxiomDescription) =
        function
        // x + y + z == x + (y + z)
        | Join joinOp (Join joinOp (a1, a2), a3), Join joinOp (b1, Join joinOp (b2, b3)) when sequal3 a1 a2 a3 b1 b2 b3 -> Some desc
        // x * y * z == x * (y * z)
        | Meet meetOp (Meet meetOp (a1, a2), a3), Meet meetOp (b1, Meet meetOp (b2, b3)) when sequal3 a1 a2 a3 b1 b2 b3 -> Some desc        
        
        | _ -> None
     
    /// Commutativity axioms.
    let (|Commute|_|) (joinOp: 't->'t->'t) (meetOp: 't->'t->'t) (desc: AxiomDescription) =
        function
        // x + y == y + x
        | Join joinOp (a1, a2), Join joinOp (b1, b2) when sequal2 a1 a2 b2 b1 -> Some desc     
        // x * y == y * x
        | Meet meetOp (a1, a2), Meet meetOp (b1, b2) when sequal2 a1 a2 b2 b1 -> Some desc  
        | _ -> None

    /// Identity axioms.
    let (|Identity|_|) (joinOp: 't->'t->'t) (z:'t) (meetOp: 't->'t->'t) (o:'t) (desc: AxiomDescription) = 
        function
        | Join joinOp (a1, Zero z _), a2 when sequal a1 a2  -> Some desc
        | Meet meetOp (a1, One o _), a2 when sequal a1 a2  -> Some desc
        | _ -> None

    /// Distributivity axioms.
    let (|Distrib|_|) (joinOp: 't->'t->'t) (meetOp: 't->'t->'t) (desc: AxiomDescription) = 
        function
        // x * (y + z) == x * y + x * z
        | Meet meetOp (a3, Join joinOp (b3, b4)), Join joinOp (Meet meetOp (a1, b1), Meet meetOp (a2, b2)) when (sequal a1 a2) && (sequal a1 a3) && sequal2 b1 b2 b3 b4 -> Some <@@ (%%a1 * %%b1) + (%%a2 * %%b2) @@>
        | _ -> None

//type BooleanAlgebra<'t when 't : equality>(join: 't->'t->'t, meet: 't->'t->'t, greatest:'t, least:'t, complement:'t -> 't) = 
//    inherit Theory()