namespace Sylph

open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns

open Sylvester
open FormulaPatterns

/// Theory of algebraic operations on a ring of integers with binary operations (+) and (*), identities 0 and 1, 
/// and unary inverse operation (-).
module IntegerAlgebra =      
    (* Patterns *)

    let (|Zero|_|) =
        function
        | SByte 0y
        | Byte 0uy
        | Int16 0s
        | UInt16 0us
        | Int32 0
        | UInt32 0u
        | Int64 0L
        | UInt64 0UL -> Some 0
        | _ -> None

    let (|One|_|) =
        function
        | SByte 1y
        | Byte 1uy
        | Int16 1s
        | UInt16 1us
        | Int32 1
        | UInt32 1u
        | Int64 1L
        | UInt64 1UL -> Some 1
        | _ -> None

    (* Axioms *)

    /// Commutativity axioms
    let (|Commute|_|) =
        function
        // x + y == y + x
        | Add(a1, a2), Add(b1, b2) when sequal2 a1 a2 b2 b1 -> Some (axiom_desc "Commutativity" <@@ fun() -> %%a1 + %%a2 = %%a2 + %%a1 @@>)  
        // x * y == y * x
        | Multiply(a1, a2), Multiply(b1, b2) when sequal2 a1 a2 b2 b1 -> Some (axiom_desc "Commutativity" <@@ fun()-> %%a1 * %%a2 = %%a2 * %%a1 @@>)  
        | _ -> None

    /// Associativity axioms
    let (|Assoc|_|) =
        function
        // x + y + z == x + (y + z)
        | Add(Add(a1, a2), a3), Add(b1, Add(b2, b3)) when sequal3 a1 a2 a3 b1 b2 b3 -> Some (axiom_desc "Associativity" <@@fun() -> (%%b1 + %%b2) + %%b3 =  %%b1 + (%%b2 + %%b3) @@>)        
        // x * y * z == x * (y * z)
        | Multiply(Multiply(a1, a2), a3), Multiply(b1, Multiply(b2, b3)) when sequal3 a1 a2 a3 b1 b2 b3 -> Some (axiom_desc "Associativity" <@@fun() -> (%%b1 * %%b2 * %%b3) = %%b1 * (%%b2 * %%b3) @@>)
        | _ -> None

    /// Distributivity axiom
    let (|Distrib|_|) =
        function
        // x * (y + z) == x * y + x * z
        | Multiply(a3, Add(b3, b4)), Add(Multiply(a1, b1), Multiply(a2, b2)) when (sequal a1 a2) && (sequal a1 a3) && sequal2 b1 b2 b3 b4 -> Some (axiom_desc "Distributivity" <@@fun() -> %%a1 * (%%b3 * %%b4) = (%%a1 * %%b1) + (%%a2 * %%b2) @@>)
        | _ -> None

    /// Identity axioms
    let (|Identity|_|) = 
        function
        // x + 0 = x
        | Add(a1, Zero zero), a2 when sequal a1 a2 -> Some (axiom_desc "Identity" <@@fun() -> %%a1 + zero = %%a1@@>)
        // x * 1 = x
        | Multiply(a1, One one), a2 when sequal a1 a2 -> Some (axiom_desc "Identity" <@@fun() -> %%a1 * one = %%a1 @@>)
        | _ -> None

    let (|IntegerAxioms|_|) =
        function
        | Commute x 
        | Assoc x
        | Distrib x
        | Identity x -> Some x
        | _ -> None
    
    let (|SymmIntegerAxioms|_|) =
        function
        | Symm(A, B) -> match (A, B) with | IntegerAxioms x -> Some x | _ -> None

    let integer_axioms = 
        function  
        | IntegerAxioms x
        | Conj(IntegerAxioms x)
        | SymmIntegerAxioms x -> Some x
        | _ -> None
                
    let rec reduce_constants  =
        function
        | Add(SByte l, SByte r) -> Expr.Value(l + r)
        | Add(Byte l, Byte r) -> Expr.Value(l + r)
        | Add(Int16 l, Int16 r) -> Expr.Value(l + r)
        | Add(UInt16 l, UInt16 r) -> Expr.Value(l + r)
        | Add(Int32 l, Int32 r) -> Expr.Value(l + r)
        | Add(UInt32 l, UInt32 r) -> Expr.Value(l + r)
        | Add(Int64 l, Int64 r) -> Expr.Value(l + r)
        | Add(UInt64 l, UInt64 r) -> Expr.Value(l + r)
        
        | Subtract(SByte l, SByte r) -> Expr.Value(l - r)
        | Subtract(Byte l, Byte r) -> Expr.Value(l - r)
        | Subtract(Int16 l, Int16 r) -> Expr.Value(l - r)
        | Subtract(UInt16 l, UInt16 r) -> Expr.Value(l - r)
        | Subtract(Int32 l, Int32 r) -> Expr.Value(l - r)
        | Subtract(UInt32 l, UInt32 r) -> Expr.Value(l - r)
        | Subtract(Int64 l, Int64 r) -> Expr.Value(l - r)
        | Subtract(UInt64 l, UInt64 r) -> Expr.Value(l - r)
        
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
    let integer_algebra = Theory(integer_axioms, [
            Reduce 
            LeftAssoc 
            RightAssoc 
            Commute 
            Distrib
            Collect
    ], id)
