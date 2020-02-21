namespace Sylvester.Prover

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns

open Sylvester
open FormulaPatterns

module IntegerArithmetic =
    
    // x + y, y + x
    let (|Commute|_|) =
        function
        | Add(a1, a2), Add(b1, b2) when sequal2 a1 a2 b2 b1 -> Some true
        | Lambda(_, Add(a1, a2)), Lambda(_, Add(b1, b2)) when sequal2 a1 a2 b2 b1 -> Some true

        | Multiply(a1, a2), Multiply(b1, b2) when sequal2 a1 a2 b2 b1 -> Some true  
        | Lambda(_, Multiply(a1, a2)), Lambda(_, Multiply(b1, b2)) when sequal2 a1 a2 b2 b1 -> Some true
        | _ -> None

    // x + y + z, x + (y + z)
    let (|Assoc|_|) =
        function
        | Add(Add(a1, a2), a3), Add(b1, Add(b2, b3)) when (sequal3 a1 a2 a3 b1 b2 b3) -> Some true
        | Lambda(_,Add(Add(a1, a2), a3)), Lambda(_, Add(b1, Add(b2, b3))) when (sequal3 a1 a2 a3 b1 b2 b3) -> Some true
        
        | Add(a1, Add(a2, a3)), Add(Add(b1, b2), b3) when (sequal3 a1 a2 a3 b1 b2 b3)-> Some true
        | Lambda(_, Add(a1, Add(a2, a3))), Lambda(_, Add(Add(b1, b2), b3)) when (sequal3 a1 a2 a3 b1 b2 b3)-> Some true
        
        | Multiply(Multiply(a1, a2), a3), Multiply(b1, Multiply(b2, b3)) when (sequal3 a1 a2 a3 b1 b2 b3) -> Some true
        | Lambda(_,Multiply(Multiply(a1, a2), a3)), Lambda(_, Multiply(b1, Multiply(b2, b3))) when (sequal3 a1 a2 a3 b1 b2 b3) -> Some true
        
        | Multiply(a1, Multiply(a2, a3)), Multiply(Multiply(b1, b2), b3) when (sequal3 a1 a2 a3 b1 b2 b3)-> Some true
        | Lambda(_, Multiply(a1, Multiply(a2, a3))), Lambda(_, Multiply(Multiply(b1, b2), b3)) when (sequal3 a1 a2 a3 b1 b2 b3)-> Some true
        
        | _ -> None

    // x * (y + z), x * y + x * z
    let (|Distrib|_|) =
        function
        | Lambda(_, Add(Multiply(a1, b1), Multiply(a2, b2))), Lambda(_, Multiply(a3, Add(b3, b4))) when (sequal a1 a2) && (sequal a1 a3) && sequal2 b1 b2 b3 b4 -> Some true
        | Add(Multiply(a1, b1), Multiply(a2, b2)), Multiply(a3, Add(b3, b4)) when (sequal a1 a2) && (sequal a1 a3) && sequal2 b1 b2 b3 b4 -> Some true
        | _ -> None

    let integer_axioms (A, B) = 
        match (A, B) with
        | Equal x  
        | Assoc x 
        | Commute x  
        | Distrib x -> true
        | _ -> false
        ||
        match (B, A) with
        | Assoc x 
        | Commute x  
        | Distrib x -> true
        | _ -> false
        
    let rec equal_constants  =
        function
        | Add(Int32 l, Int32 r) -> Expr.Value(l + r)
        | Subtract(Int32 l, Int32 r) -> Expr.Value(l - r)
        | Multiply(Int32 l, Int32 r) -> Expr.Value(l * r)
        | expr -> traverse expr equal_constants
    
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

    let rec commute_right =
        function
        | Call(Some o, m, l::Add(a1, a2)::[]) -> let s = <@@ %%a2 + %%a1 @@> in Expr.Call(o, m, l::s::[])
        | Call(None, m, l::Add(a1, a2)::[]) -> let s = <@@ %%a2 + %%a1 @@> in Expr.Call(m, l::s::[])
        | Call(Some o, m, l::Multiply(a1, a2)::[]) -> let s = <@@ %%a2 * %%a1 @@> in Expr.Call(o, m, l::s::[])
        | Call(None, m, l::Multiply(a1, a2)::[]) -> let s = <@@ %%a2 * %%a1 @@> in Expr.Call(m, l::s::[])
        | expr -> traverse expr commute_right

    let rec commute_left =
        function
        | Call(Some o, m, Add(a1, a2)::r::[]) -> let s = <@@ %%a2 + %%a1 @@> in Expr.Call(o, m, s::r::[])
        | Call(None, m, Add(a1, a2)::r::[]) -> let s = <@@ %%a2 + %%a1 @@> in Expr.Call(m, s::r::[])
        | Call(Some o, m, Multiply(a1, a2)::r::[]) -> let s = <@@ %%a2 * %%a1 @@> in Expr.Call(o, m, s::r::[])
        | Call(None, m, Multiply(a1, a2)::r::[]) -> let s = <@@ %%a2 * %%a1 @@> in Expr.Call(m, s::r::[])
        | expr -> traverse expr commute_left

    let rec left_assoc_right =
        function
        | Call(None, m, l::Add(a1, Add(a2, a3))::[]) -> let s = <@@ (%%a1 + %%a2) + %%a3 @@> in Expr.Call(m, l::s::[])
        | Call(Some o, m, l::Add(a1, Add(a2, a3))::[]) -> let s = <@@ (%%a1 + %%a2) + %%a3 @@> in Expr.Call(o, m, l::s::[])
        | Call(None, m, l::Multiply(a1, Multiply(a2, a3))::[]) -> let s = <@@ (%%a1 * %%a2) * %%a3 @@> in Expr.Call(m, l::s::[])
        | Call(Some o, m, l::Multiply(a1, Multiply(a2, a3))::[]) -> let s = <@@ (%%a1 * %%a2) * %%a3 @@> in Expr.Call(o, m, l::s::[])
        | expr -> traverse expr left_assoc_right

    let rec left_assoc_left =
        function
        | Call(None, m, Add(a1, Add(a2, a3))::r::[]) -> let s = <@@ (%%a1 + %%a2) + %%a3 @@> in Expr.Call(m, s::r::[])
        | Call(Some o, m, Add(a1, Add(a2, a3))::r::[]) -> let s = <@@ (%%a1 + %%a2) + %%a3 @@> in Expr.Call(o, m, s::r::[])
        | Call(None, m, Multiply(a1, Multiply(a2, a3))::r::[]) -> let s = <@@ (%%a1 * %%a2) * %%a3 @@> in Expr.Call(m, s::r::[])
        | Call(Some o, m, Multiply(a1, Multiply(a2, a3))::r::[]) -> let s = <@@ (%%a1 * %%a2) * %%a3 @@> in Expr.Call(o, m, s::r::[])
        | expr -> traverse expr left_assoc_left
    
    let rec distrib =
        function
        | Multiply(a1, Add(a2, a3)) -> <@@ %%a1 * %%a2 + %%a1 * %%a3 @@> 
        | Multiply(a1, Subtract(a2, a3)) -> <@@ %%a1 * %%a2 - %%a1 * %%a3 @@> 
        | expr -> traverse expr distrib

    let rec distrib_left =
        function
        | Call(None, m, Multiply(a1, Add(a2, a3))::r::[]) -> Expr.Call(m, let s = <@@ %%a1 * %%a2 + %%a1 * %%a3 @@> in s::r::[])
        | Call(Some o, m, Multiply(a1, Add(a2, a3))::r::[]) -> Expr.Call(o, m, let  s = <@@ %%a1 * %%a2 + %%a1 * %%a3 @@> in s::r::[])
        | Call(None, m, Multiply(a1, Subtract(a2, a3))::r::[]) -> Expr.Call(m, let s = <@@ %%a1 * %%a2 - %%a1 * %%a3 @@> in s::r::[])
        | Call(Some o, m, Multiply(a1, Subtract(a2, a3))::r::[]) -> Expr.Call(o, m, let s = <@@ %%a1 * %%a2 - %%a1 * %%a3 @@> in s::r::[])
        | expr -> traverse expr distrib_left

    let rec distrib_right =
        function
        | Call(None, m, l::Multiply(a1, Add(a2, a3))::[]) -> Expr.Call(m, let s = <@@ %%a1 * %%a2 + %%a1 * %%a3 @@> in l::s::[])
        | Call(Some o, m, l::Multiply(a1, Add(a2, a3))::[]) -> Expr.Call(o, m, let  s = <@@ %%a1 * %%a2 + %%a1 * %%a3 @@> in l::s::[])
        | Call(None, m, l::Multiply(a1, Subtract(a2, a3))::[]) -> Expr.Call(m, let s = <@@ %%a1 * %%a2 - %%a1 * %%a3 @@> in l::s::[])
        | Call(Some o, m, l::Multiply(a1, Subtract(a2, a3))::[]) -> Expr.Call(o, m, let s = <@@ %%a1 * %%a2 - %%a1 * %%a3 @@> in l::s::[])
        | expr -> traverse expr distrib_right

    let rec collect =
        function
        | Add(Multiply(a1, a2), Multiply(a3, a4)) when sequal a1 a3 -> <@@ %%a1 * (%%a2 + %%a4) @@>
        | Add(Multiply(a1, a2), Multiply(a3, a4)) when sequal a2 a4 -> <@@ %%a2 * (%%a1 + %%a3) @@>
        
        | Subtract(Multiply(a1, a2), Multiply(a3, a4)) when sequal a1 a3 -> <@@ %%a1 * (%%a2 - %%a4) @@>
        | Subtract(Multiply(a1, a2), Multiply(a3, a4)) when sequal a2 a4 -> <@@ %%a2 * (%%a1 - %%a3) @@>
        | expr -> traverse expr collect

    let rec collect_left =
        function
        | Call(None, m, Add(Multiply(a1, a2), Multiply(a3, a4))::r::[]) when sequal a1 a3 -> let s = <@@ %%a1 * (%%a2 + %%a4) @@> in Expr.Call(m, s::r::[])
        | Call(Some o, m, Add(Multiply(a1, a2), Multiply(a3, a4))::r::[]) when sequal a1 a3 -> let s = <@@ %%a1 * (%%a2 + %%a4) @@> in Expr.Call(o, m, s::r::[])
        
        | Call(None, m, Add(Multiply(a1, a2), Multiply(a3, a4))::r::[]) when sequal a2 a4 -> let s = <@@ %%a2 * (%%a1 + %%a3) @@> in Expr.Call(m, s::r::[])
        | Call(Some o, m, Add(Multiply(a1, a2), Multiply(a3, a4))::r::[]) when sequal a2 a4 -> let s = <@@ %%a2 * (%%a1 + %%a3) @@> in Expr.Call(o, m, s::r::[])

        | Call(None, m, Subtract(Multiply(a1, a2), Multiply(a3, a4))::r::[]) when sequal a1 a3 -> let s = <@@ %%a1 * (%%a2 - %%a4) @@> in Expr.Call(m, s::r::[])
        | Call(Some o, m, Subtract(Multiply(a1, a2), Multiply(a3, a4))::r::[]) when sequal a1 a3 -> let s = <@@ %%a1 * (%%a2 - %%a4) @@> in Expr.Call(o, m, s::r::[])

        | Call(None, m, Subtract(Multiply(a1, a2), Multiply(a3, a4))::r::[]) when sequal a2 a4 -> let s = <@@ %%a2 * (%%a1 - %%a3) @@> in Expr.Call(m, s::r::[])
        | Call(Some o, m, Subtract(Multiply(a1, a2), Multiply(a3, a4))::r::[]) when sequal a2 a4 -> let s = <@@ %%a2 * (%%a1 - %%a3) @@> in Expr.Call(o, m, s::r::[])
        | expr -> traverse expr collect_left

    let rec collect_right =
        function
        | Call(None, m, l::Add(Multiply(a1, a2), Multiply(a3, a4))::[]) when sequal a1 a3 -> let s = <@@ %%a1 * (%%a2 + %%a4) @@> in Expr.Call(m, l::s::[])
        | Call(Some o, m, l::Add(Multiply(a1, a2), Multiply(a3, a4))::[]) when sequal a1 a3 -> let s = <@@ %%a1 * (%%a2 + %%a4) @@> in Expr.Call(o, m, l::s::[])
        
        | Call(None, m, Subtract(Multiply(a1, a2), Multiply(a3, a4))::r::[]) when sequal a2 a4 -> let s = <@@ %%a2 * (%%a1 - %%a3) @@> in Expr.Call(m, s::r::[])
        | Call(Some o, m, Subtract(Multiply(a1, a2), Multiply(a3, a4))::r::[]) when sequal a2 a4 -> let s = <@@ %%a2 * (%%a1 - %%a3) @@> in Expr.Call(o, m, s::r::[])

        | Call(None, m, l::Subtract(Multiply(a1, a2), Multiply(a3, a4))::[]) when sequal a1 a3 -> let s = <@@ %%a1 * (%%a2 - %%a4) @@> in Expr.Call(m, l::s::[])
        | Call(Some o, m, l::Subtract(Multiply(a1, a2), Multiply(a3, a4))::[]) when sequal a1 a3 -> let s = <@@ %%a1 * (%%a2 - %%a4) @@> in Expr.Call(o, m, l::s::[])

        | Call(None, m, l::Subtract(Multiply(a1, a2), Multiply(a3, a4))::[]) when sequal a2 a4 -> let s = <@@ %%a2 * (%%a1 - %%a3) @@> in Expr.Call(m, l::s::[])
        | Call(Some o, m, l::Subtract(Multiply(a1, a2), Multiply(a3, a4))::[]) when sequal a2 a4 -> let s = <@@ %%a2 * (%%a1 - %%a3) @@> in Expr.Call(o, m, l::s::[])

        | expr -> traverse expr collect_right

    /// Reduce equal constants in A and B. 
    let equal_constants_a_b = Rule("Reduce equal constants in A and B", fun (a,b) -> equal_constants a, equal_constants b)

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

    /// Left side of A is commutative.
    let commute_a_left = Rule("Left side of A is commutative", fun (a, b) -> commute_left a, b)

    /// Right side of A is commutative.
    let commute_a_right = Rule("Right side of A is commutative", fun (a, b) -> commute_right a, b)

    /// Left side of B is commutative.
    let commute_b_left = Rule("Left side of B is commutative", fun (a, b) -> a, commute_left b)

    /// Right side of B is commutative.
    let commute_b_right = Rule("Right side of B is commutative", fun (a, b) -> a, commute_right b)

    /// Left side of A is left_associative.
    let left_assoc_a_left = Rule("Left side of A is left-associative", fun (a, b) -> left_assoc_left a, b)

    /// Right side of A is left_associative.
    let left_assoc_a_right = Rule("Right side of A is left-associative", fun (a, b) -> left_assoc_right a, b)

    /// Left side of B is left_associative.
    let left_assoc_b_left = Rule("Left side of B is left-associative", fun (a, b) -> a, left_assoc_left b)

    /// Right side of B is left_associative.
    let left_assoc_b_right = Rule("Right side of B is left-associative", fun (a, b) -> a, left_assoc_right b)

    /// Multiplication distributes over addition in A.
    let distrib_a = Rule("Multiplication distributes over addition in A", fun(a,b) -> distrib a, b)
    
    /// Multiplication distributes over addition in B.
    let distrib_b = Rule("Multiplication distributes over addition in B", fun(a,b) -> a, distrib b)

    /// Multiplication distributes over addition in left side of A.
    let distrib_a_left = Rule("Multiplication distributes over addition in left side of A", fun(a,b) -> distrib_left a, b)
    
    /// Multiplication distributes over addition in right side of A.
    let distrib_a_right = Rule("Multiplication distributes over addition in right-side of A", fun(a,b) -> distrib_right a, b)

    /// Multiplication distributes over addition in left side of B.
    let distrib_b_left = Rule("Multiplication distributes over addition in left side of B", fun(a,b) -> a, distrib_left b)
    
    /// Multiplication distributes over addition in right side of B.
    let distrib_b_right = Rule("Multiplication distributes over addition in right-side of B", fun(a,b) -> a, distrib_right b)

    /// Collect multiplication terms distributed over addition in A.
    let collect_a = Rule("Collect multiplication terms distributed over addition in A", fun(a,b) -> collect a, b)
    
    /// Collect multiplication terms distributed over addition in B.
    let collect_b = Rule("Collect multiplication terms distributed over addition in B", fun(a,b) -> a, collect b)

    /// Collect multiplication terms distributed over addition in left side of A.
    let collect_a_left = Rule("Collect multiplication terms distributed over addition in left-side of A", fun(a,b) -> collect_left a, b)
    
    /// Collect multiplication terms distributed over addition in right side of A.
    let collect_a_right = Rule("Collect multiplication terms distributed over addition in right-side of A", fun(a,b) -> collect_right a, b)

    /// Collect multiplication terms distributed over addition in left side of B.
    let collect_b_left = Rule("Collect multiplication terms distributed over addition in left-side of B", fun(a,b) -> a, collect_left b)
    
    /// Collect multiplication terms distributed over addition in right side of B.
    let collect_b_right = Rule("Collect multiplication terms distributed over addition in right-side of B", fun(a,b) -> a, collect_right b)

    /// Axioms and rules for integer arithmetic.
    let integer_arithmetic = 
        ProofSystem(integer_axioms, [
            equal_constants_a_b 
            left_assoc_a 
            left_assoc_b 
            right_assoc_a 
            right_assoc_b 
            left_assoc_a_left
            left_assoc_a_right
            left_assoc_b_left
            left_assoc_b_right
            commute_a 
            commute_b 
            commute_a_left
            commute_b_left
            commute_a_right
            commute_b_right
            distrib_a 
            distrib_b 
            distrib_a_left
            distrib_b_left
            distrib_a_right
            distrib_b_right
            collect_a 
            collect_b 
            collect_a_left
            collect_b_left
            collect_a_right
            collect_b_right
        ])
