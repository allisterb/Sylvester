namespace Sylph

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns

open Sylvester
open FormulaPatterns

module IntegerAlgebra =    
    // x + y, y + x
    let (|Commute|_|) =
        function
        | Add(a1, a2), Add(b1, b2) when sequal2 a1 a2 b2 b1 -> Some <@@ %%b1 + %%b2 @@>     
        | Multiply(a1, a2), Multiply(b1, b2) when sequal2 a1 a2 b2 b1 -> Some <@@ %%b1 * %%b2 @@>  
        | _ -> None

    // x + y + z, x + (y + z)
    let (|Assoc|_|) =
        function
        | Add(Add(a1, a2), a3), Add(b1, Add(b2, b3)) when sequal3 a1 a2 a3 b1 b2 b3 -> Some <@@ %%b1 + (%%b2 + %%b3) @@>        
        | Multiply(Multiply(a1, a2), a3), Multiply(b1, Multiply(b2, b3)) when sequal3 a1 a2 a3 b1 b2 b3 -> Some <@@ %%b1 * (%%b2 * %%b3) @@>
        | _ -> None

    // x * (y + z), x * y + x * z
    let (|Distrib|_|) =
        function
        | Multiply(a3, Add(b3, b4)), Add(Multiply(a1, b1), Multiply(a2, b2)) when (sequal a1 a2) && (sequal a1 a3) && sequal2 b1 b2 b3 b4 -> Some <@@ (%%a1 * %%b1) + (%%a2 * %%b2) @@>
        | _ -> None

    // x + 0 = x
    let (|AddIdentity|_|) = 
        function
        | a1, Add(a2, Int32 0) when sequal a1 a2 -> Some a1
        | Add(a1, Int32 0), a2 when sequal a1 a2 -> Some <@@ true @@>
        | _ -> None

    // x * 1 = x
    let (|MulIdentity|_|) = 
        function
        | a1, Multiply(a2, Int32 1) when sequal a1 a2 -> Some <@@ a1 @@>     
        | Multiply(a1, Int32 1), a2 when sequal a1 a2 -> Some <@@ a1 @@>
        | _ -> None

    let integer_axioms = 
        function  
        | Assoc x 
        | Commute x
        | AddIdentity x
        | MulIdentity x
        | Distrib x -> true
        | _ -> false
                
    let rec reduce_constants  =
        function
        | Add(Int32 l, Int32 r) -> Expr.Value(l + r)
        | Add(UInt32 l, UInt32 r) -> Expr.Value(l + r)
        | Add(Int64 l, Int64 r) -> Expr.Value(l + r)
        
        | Subtract(Int32 l, Int32 r) -> Expr.Value(l - r)
        | Subtract(UInt32 l, UInt32 r) -> Expr.Value(l - r)
        | Subtract(Int64 l, Int64 r) -> Expr.Value(l - r)
        
        | Multiply(Int32 l, Int32 r) -> Expr.Value(l * r)
        | Multiply(UInt32 l, UInt32 r) -> Expr.Value(l * r)
        | Multiply(Int64 l, Int64 r) -> Expr.Value(l * r)

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
    let Reduce = Rule("Reduce constants in (expression)", reduce_constants)

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
    
    /// Axioms and rules for integer algebra.
    let integer_algebra = 
        Theory(integer_axioms, [
            Reduce 
            LeftAssoc 
            RightAssoc 
            Commute 
            Distrib
            Collect
        ])
