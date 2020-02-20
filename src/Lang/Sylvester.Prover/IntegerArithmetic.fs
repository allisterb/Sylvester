namespace Sylvester.Prover

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns

open Sylvester

module IntegerArithmetic =
    open FormulaPatterns
    
    let integer_axioms = 
        function
        | Equal x  
        | Assoc x 
        | Commute x -> true 
        | _ -> false

    let rec reduce_constants  =
        function
        | Add(Int32 l, Int32 r) -> Expr.Value(l + r)
        | Add(Int32 l, Add(Int32 r1, r2)) -> let s = Expr.Value(l + r1) in <@@ %%s + %%r2 @@>
        | Subtract(Int32 l, Int32 r) -> Expr.Value(l - r)
        | Multiply(Int32 l, Int32 r) -> Expr.Value(l * r)
        | expr -> traverse expr reduce_constants
    
    let rec right_assoc =
        function
        | Add(Add(a1, a2), a3) -> <@@ %%a1 + (%%a2 + %%a3) @@>
        | Subtract(Subtract(a1, a2), a3) -> <@@ %%a1 - (%%a2 + %%a3) @@>
        | Multiply(Multiply(a1, a2), a3) -> <@@ %%a1 * (%%a2 * %%a3) @@>
        | expr -> traverse expr right_assoc

    let rec left_assoc =
        function
        | Add(a1, Add(a2, a3)) -> <@@ (%%a1 + %%a2) + %%a3 @@>
        | Subtract(a1, Subtract(a2, a3)) -> <@@ (%%a1 - %%a2) + %%a3 @@>
        | Multiply(a1, Multiply(a2, a3)) -> <@@ (%%a1 * %%a2) * %%a3 @@>
        | expr -> traverse expr left_assoc

    let rec commute =
        function
        | Add(a1, a2) -> <@@ (%%a2 + %%a1) @@>
        | Multiply(a1, a2) -> <@@ (%%a2 * %%a1) @@>
        | expr -> traverse expr commute

    let rec right_commute =
        function
        | Add(l, Add(a1, a2)) -> <@@ %%l + (%%a2 + %%a1) @@>
        | Multiply(l, Add(a1, a2)) -> <@@ %%l * (%%a2 + %%a1) @@>
        | Subtract(l, Add(a1, a2)) -> <@@ %%l - (%%a2 + %%a1) @@>
        | Add(l, Multiply(a1, a2)) -> <@@ %%l + (%%a2 * %%a1) @@>
        | Multiply(l, Multiply(a1, a2)) -> <@@ %%l * (%%a2 * %%a1) @@>
        | Subtract(l, Multiply(a1, a2)) -> <@@ %%l - (%%a2 * %%a1) @@>
        | expr -> traverse expr right_commute

    /// Reduce equal constants in A and B. 
    let reduce_constants_a_b = Rule("Reduce equal constants in A and B", fun (a,b) -> reduce_constants a, reduce_constants b)

    /// A is left associative.
    let left_assoc_a = Rule("A is left-associative", fun (a,b) -> left_assoc a, b)
    
    /// B is left associative.
    let left_assoc_b = Rule("B is left-associative", fun (a, b) -> a, left_assoc b)

    /// A is right associative.
    let right_assoc_a = Rule("A is right-associative", fun (a, b) -> right_assoc a, b)
    
    /// B is right associative.
    let right_assoc_b = Rule("B is right-associative", fun (a, b) -> a, right_assoc b)

    /// A is commutative.
    let commute_a = Rule("A is commutative", fun (a, b) -> commute a, b)

    /// B is commutative.
    let commute_b = Rule("B is commutative", fun (a, b) -> a, commute b)

    /// A is right commutative.
    let right_commute_a = Rule("A is right-commutative", fun (a, b) -> right_commute a, b)

    /// B is right commutative.
    let right_commute_b = Rule("B is right-commutative", fun (a, b) -> a, right_commute b)

    
    /// Axioms and rules for integer arithmetic.
    let IntegerArithmetic = 
        ProofSystem(integer_axioms, 
            [reduce_constants_a_b; left_assoc_a; left_assoc_b; right_assoc_a; right_assoc_b; commute_a; commute_b; right_commute_a; right_commute_b])
