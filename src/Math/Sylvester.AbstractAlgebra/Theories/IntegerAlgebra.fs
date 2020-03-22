namespace Sylph

open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns

open Sylvester
open FormulaPatterns
open FormulaDescriptions

/// Theory of algebraic operations on a ring of integers with binary operations (+) and (*), identities 0 and 1, 
/// and unary inverse operation (-).
module IntegerAlgebra =      
    let desc = axiom_desc "Integer Algebra" id
    
    (* Axioms *)

    let (|IntegerAlgebraAxioms|_|) =
        function        
        | Assoc <@ (+) @> x
        | Assoc <@ (*) @> x
        
        | Identity <@ (+) @> <@ 0 @> x 
        | Identity <@ (*) @> <@ 1 @> x
        
        | Inverse <@ (+) @> <@ (~-) @> <@ 0 @> x

        | Commute <@ (+) @> x
        | Commute <@ (*) @> x

        | Distrib <@ (*) @> <@ (+) @> x -> Some (desc x)
        | _ -> None
    
    let (|SymmIntegerAxioms|_|) =
        function
        | Symm(A, B) -> 
            match (A, B) with 
            | IntegerAlgebraAxioms x -> Some x 
            | Conj(IntegerAlgebraAxioms x) -> Some x
            | _ -> None

    let integer_algebra_axioms = 
        function  
        | IntegerAlgebraAxioms x
        | Conj(IntegerAlgebraAxioms x)
        | SymmIntegerAxioms x -> Some x
        | _ -> None
                
    let rec reduce_constants  =
        function
        | Add(Int32 l, Int32 r) -> Expr.Value(l + r)
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

    let rec distrib =
        function
        | Multiply(a1, Add(a2, a3)) -> <@@ %%a1 * %%a2 + %%a1 * %%a3 @@> 
        | Multiply(a1, Subtract(a2, a3)) -> <@@ %%a1 * %%a2 - %%a1 * %%a3 @@> 
        | expr -> traverse expr distrib

    let rec collect =
        function
        | Add(Multiply(a1, a2), Multiply(a3, a4)) when sequal a1 a3 -> <@@ %%a1 * (%%a2 + %%a4) @@>
        | Add(Multiply(a1, a2), Multiply(a3, a4)) when sequal a2 a4 -> <@@ %%a2 * (%%a1 + %%a3) @@>
        
        | Subtract(Multiply(a1, a2), Multiply(a3, a4)) when sequal a1 a3 -> <@@ %%a1 * (%%a2 - %%a4) @@>
        | Subtract(Multiply(a1, a2), Multiply(a3, a4)) when sequal a2 a4 -> <@@ %%a2 * (%%a1 - %%a3) @@>
        | expr -> traverse expr collect

    /// Reduce equal constants in expression. 
    let Reduce = Rule("Reduce integer constants in (expression)", reduce_constants)

    /// Expression is left associative.
    let LeftAssoc = Rule("(expression) is left-associative", left_assoc)
    
    /// Expression is right associative.
    let RightAssoc = Rule("(expression) is right-associative", right_assoc)
      
    /// Expression is commutative.
    let Commute = Rule("(expression) is commutative", commute)

    /// Multiplication distributes over addition in expression.
    let Distrib = Rule("Multiplication distributes over addition in (expression)", distrib)
    
    /// Collect multiplication terms distributed over addition in expression.
    let Collect = Rule("Collect multiplication terms distributed over addition in (expression)", collect)
    
    /// Theory of algebraic operations on a ring of integers with binary operations (+) and (*), identities 0 and 1, 
    /// and unary inverse operation (~-).
    let integer_algebra = Theory(integer_algebra_axioms, [
            Reduce 
            LeftAssoc 
            RightAssoc 
            Commute 
            Distrib
            Collect
    ], id)
