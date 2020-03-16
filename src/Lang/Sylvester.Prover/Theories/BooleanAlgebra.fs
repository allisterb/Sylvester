namespace Sylph

open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns

open Sylvester
open FormulaPatterns
open EquivPatterns

/// Theory of Boolean algebra on a set closed under 2 binary operations that are associative, commutative, and idempotent,
/// wiith identity elements 0 and 1, and a unary inverse or complement operation.
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

    /// Order operation induced by closure of set under join or meet operations.
    let (|Order|_|) (op:'t->'t->bool) =
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
    let (|Assoc|_|) (join: 't->'t->'t) (meet: 't->'t->'t) =
        function
        // x + y + z == x + (y + z)
        | Join join (Join join (a1, a2), a3), Join join (b1, Join join (b2, b3)) when sequal3 a1 a2 a3 b1 b2 b3 -> 
                                                                                        Some (axiom_desc "Associativity" <@fun (x:'t) (y:'t) (z:'t) -> join (join x y) z = join x (join y z)   @>) 
        // x * y * z == x * (y * z)
        | Meet meet (Meet meet (a1, a2), a3), Meet meet (b1, Meet meet (b2, b3)) when sequal3 a1 a2 a3 b1 b2 b3 ->
                                                                                        Some (axiom_desc "Associativity" <@fun (x:'t) (y:'t) (z:'t) -> meet (meet x y) z = meet x (meet y z) @>)
        | _ -> None
     
    /// Commutativity axioms.
    let (|Commute|_|) (join: 't->'t->'t) (meet: 't->'t->'t) =
        function
        // x + y == y + x
        | Join join (a1, a2), Join join (b1, b2) when sequal2 a1 a2 b2 b1 -> Some (axiom_desc "Commutativity" <@fun (x:'t) (y:'t) -> join x y = join y x @>)     
        // x * y == y * x
        | Meet meet (a1, a2), Meet meet (b1, b2) when sequal2 a1 a2 b2 b1 -> Some (axiom_desc "Commutativity" <@fun (x:'t) (y:'t) -> meet x y = meet y x @>)  
        | _ -> None

    /// Identity axioms.
    let (|Identity|_|) (join: 't->'t->'t) (meet: 't->'t->'t) (zero:'t) (one:'t)  = 
        function
        // x + 0 == x
        | Join join (a1, Zero zero _), a2 when sequal a1 a2  -> Some (axiom_desc "Identity" <@ fun (x:'t) -> join x zero = zero @>)
        // x * 1 = x
        | Meet meet (a1, One one _), a2 when sequal a1 a2  -> Some (axiom_desc "Identity" <@ fun (x:'t) -> meet x one = x @>)
        // 1 <> 0
        | Equiv((One one _), (Zero zero _)), Bool true -> Some (axiom_desc "Identity" <@ fun () -> one <> zero @>)
        | _ -> None

    /// Idempotent axioms
    let (|Idempotent|_|) (join: 't->'t->'t)(meet: 't->'t->'t)   = 
        function
        // x + a == x
        | Join join (a1, a2) when sequal a1 a2  -> Some (axiom_desc "Idempotent" <@ fun (x:'t) -> join x x = x @>)
        // x * 1 = x
        | Meet meet (a1, a2) when sequal a1 a2  -> Some (axiom_desc "Idempotent" <@ fun (x:'t) -> meet x x = x @>)
        
        | _ -> None
        
    /// Inverse axioms.
    let (|Inverse|_|) (join: 't->'t->'t) (meet: 't->'t->'t) (zero:'t) (one:'t) (comp:'t -> 't)  =
        function
        | Join join (a1, Comp comp (a2)), One one _ when sequal a1 a2 -> Some (axiom_desc "Inverse" <@ fun (x:'t) -> join x (comp x) = zero @>)
        | Meet meet (a1, Comp comp (a2)), Zero zero _ when sequal a1 a2 -> Some (axiom_desc "Inverse" <@ fun (x:'t) -> meet x (comp x) = one @>)
        | _ -> None

    /// Distributivity axioms.
    let (|Distrib|_|) (join: 't->'t->'t) (meet: 't->'t->'t)  = 
        function
        // x * (y + z) == x * y + x * z
        | Meet meet (a3, Join join (b3, b4)), Join join (Meet meet (a1, b1), Meet meet (a2, b2)) when (sequal a1 a2) && (sequal a1 a3) && sequal2 b1 b2 b3 b4 -> 
                                                                                       Some (axiom_desc "Distributivity" <@ fun (x:'t) (y:'t) (z:'t) -> meet x (join y z) = join (meet x y) (meet x z) @>)
        // x + (y * z) == x * y + x * z
        | Join join (a3, Meet meet (b3, b4)), Meet meet (Join join (a1, b1), Join join (a2, b2)) when (sequal a1 a2) && (sequal a1 a3) && sequal2 b1 b2 b3 b4 -> 
                                                                                        Some (axiom_desc "Distributivity" <@ fun (x:'t) (y:'t) (z:'t) -> join x (meet y z) =meet (join x y) (join x z) @>)
        | _ -> None

    (* Rules *)

    let rec reduce_idemp (join: 't->'t->'t) (meet: 't->'t->'t) (zero:'t)  (one:'t) (comp:'t -> 't) =
        function
        | Join join (a1, a2) when sequal a1 a2 -> <@@ %%a1 @@>
        | Meet meet (a1, a2) when sequal a1 a2 -> <@@ %%a1 @@> 
        | Join join (a1, Comp comp a2) when sequal a1 a2 -> <@@ zero @@>
        | Meet join (a1, Comp comp a2) when sequal a1 a2 -> <@@ one @@>
        | expr -> traverse expr (reduce_idemp join meet zero one comp)

    let rec reduce_ident (join: 't->'t->'t) (meet: 't->'t->'t) (zero:'t)  (one:'t) (comp:'t -> 't) =
        function
        | Join join (a1, Zero zero _) -> <@@ %%a1 @@>
        | Meet meet (a1, One one _) -> <@@ %%a1 @@>
        | expr -> traverse expr (reduce_ident join meet zero one comp)

    let rec reduce_comp (join: 't->'t->'t) (meet: 't->'t->'t) (zero:'t) (one:'t) (comp:'t -> 't) =
        function
        | Join join (a1, Comp comp a2) when sequal a1 a2 -> <@@ zero @@>
        | Meet join (a1, Comp comp a2) when sequal a1 a2 -> <@@ one @@>
        | expr -> traverse expr (reduce_comp join meet zero one comp)

    let rec right_assoc (join: 't->'t->'t) (meet: 't->'t->'t) =
        function
        | Join join (Join join (a1, a2), a3) -> <@@ join %%a1  (join %%a2 %%a3) @@>
        | Meet meet (Meet meet (a1, a2), a3) -> <@@ join %%a1  (join %%a2 %%a3) @@>
        | expr -> traverse expr (right_assoc join meet)

    let rec left_assoc (join: 't->'t->'t) (meet: 't->'t->'t) =
        function
        | Join join (a1, Join join (a2, a3)) -> <@@ join (join %%a1 %%a2) %%a3 @@>
        | Meet meet (a1, Meet meet (a2, a3)) -> <@@ meet (meet %%a1 %%a2) %%a3 @@>
        | expr -> traverse expr (left_assoc meet meet)

    let rec commute (join: 't->'t->'t) (meet: 't->'t->'t) =
        function
        | Join join (a1, a2) -> <@@ join %%a2 %%a1 @@>
        | Meet meet (a1, a2) -> <@@ meet %%a2 %%a1 @@>
        | expr -> traverse expr (commute join meet)

    let rec distrib (join: 't->'t->'t) (meet: 't->'t->'t) =
        function
        // x * ( y + z) == x * y + x * z
        | Meet meet (a1, Join join (a2, a3)) -> <@@ join (meet %%a1 %%a2)  (meet %%a1 %%a3) @@> 
        // x + ( y * z) == x + y * x * z
        | Join join (a1, Meet meet (a2, a3)) -> <@@ meet (join %%a1 %%a2)  (join %%a1 %%a3) @@> 
        | expr -> traverse expr (distrib join meet)

    let (|BooleanAlgebraAxioms|_|) (join: 't->'t->'t) (meet: 't->'t->'t) (zero:'t) (one:'t) (comp: 't->'t) = 
        function
        | Assoc join meet x
        | Commute join meet x
        | Identity join meet zero one x
        | Inverse join meet zero one comp x
        | Distrib join meet x -> Some x
        | _ -> None

    let (|SymmBooleanAlgebraAxioms|_|) (join: 't->'t->'t) (meet: 't->'t->'t) (zero:'t) (one:'t) (comp: 't->'t) =
        function
        | Symm(A, B) -> match (A, B) with | BooleanAlgebraAxioms join meet zero one comp x -> Some x | _ -> None

    let boolean_algebra_axioms (join: 't->'t->'t) (meet: 't->'t->'t) (zero:'t) (one:'t) (comp: 't->'t) = 
        function  
        | BooleanAlgebraAxioms join meet zero one comp x
        | Conj(BooleanAlgebraAxioms join meet zero one comp x)
        | SymmBooleanAlgebraAxioms join meet zero one comp x -> Some x
        | _ -> None

    /// Idempotent operation in (expression) can be reduced.
    let ReduceIdemp join meet zero one comp = Rule("Idempotent operation in (expression) can be reduced", reduce_idemp join meet zero one comp)
    
    /// Identity in (expression) can be reduced.
    let ReduceIdent join meet zero one comp = Rule("Identity in (expression) can be reduced", reduce_ident join meet zero one comp)

    /// Complement operation in (expression) can be reduced.
    let ReduceComp join meet zero one comp = Rule("Complement operation in (expression) can be reduced", reduce_comp join meet zero one comp)
    
    /// Expression is left-associative.
    let LeftAssoc join meet = Rule("(expression) is left-associative", left_assoc join meet)
    
    /// Expression is right associative.
    let RightAssoc join meet = Rule("(expression) is right-associative", right_assoc join meet)

    /// Expression is commutative.
    let Commute join meet = Rule("(expression) is commutative", commute join meet)

    /// Expression is commutative.
    let Distrib join meet = Rule("(expression) is distributive", distrib join meet)

    //type BooleanAlgebra<'t when 't: equality>(join: 't->'t->'t, meet: 't->'t->'t, zero:'t, one:'t, comp: 't->'t) = 
    //    inherit Theory(boolean_algebra_axioms join meet zero one comp, 
    //        [
    //            
    //        ], id) 
        