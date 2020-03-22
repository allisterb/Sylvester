namespace Sylvester

open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns

open FormulaDescriptions

/// Logical operators for formulas.
[<AutoOpen>]
module Operators =
    let (|&|) (l:bool) (r:bool) = l && r
    let (|||) (l:bool) (r:bool) = l || r
    let (==>) (l:bool) (r:bool) = (not l) || r
    let (<==) (l:bool) (r:bool) = r ==> l

/// Formula patterns used in axioms
module FormulaPatterns =    
    // The (=) operator is logical equivalence which is associative i.e we can say a = b = c.
    // The == operator is conjunctional equality: A == B == C means A == B and A == C.
    // This is the opposite convention to what Gries et.al adopts for equational logic but we must
    // do it this way because of limitations on how we can use the F# (=) operator. 
    let (|Equiv|_|) = 
         function
         | SpecificCall <@@ (=) @@> (None,_,l::r::[]) -> Some(l, r)
         | _ -> None
        
    /// We must define patterns for axioms conjunctively also.
    let (|Conj|_|) =
        function
        | Equiv(expr2), Bool true -> Some expr2
        | _ -> None

    /// We must define patterns for axioms symmetrically also.
    let (|Symm|):(Expr * Expr)->(Expr * Expr) =
        function
        | (A, B) -> (B, A)

    /// Main axiom of Sylph's symbolic equality. A and B are equal if they are: 
    /// * Syntactically valid F# expressions
    /// * Decomposed to the same sequence of symbols i.e. strings.
    /// Since we are only concerned with string equality this law encompasses all 4 of the equational logic laws of equality:
    /// Symmetry, reflexivity, transitivity, and Leibniz's rule: A = B => S(A) = S(B)
    let (|SEqual|_|) =
        function
        | (A, B) when sequal A B -> Some (pattern_desc "Equality" <@ fun x y -> x  = y @>)
        | _ -> None

    let (|Not|_|) =
        function
        | SpecificCall <@@ not @@> (None,_,l::[]) -> Some l
        | _ -> None

    let (|NotEquiv|_|) =
         function
         | Not(Equiv(l, r)) -> Some (l, r)
         | SpecificCall <@@ (<>) @@> (None,_,l::r::[]) -> Some (l, r)
         | _ -> None

    let (|And|_|)  =
        function
        | SpecificCall <@@ (|&|) @@> (None,_,l::r::[]) -> Some (l,r)
        | _ -> None

    let (|Or|_|) =
        function
        | SpecificCall <@@ (|||) @@> (None,_,l::r::[]) -> Some (l,r)
        | _ -> None

    let (|Implies|_|) =
        function
        | SpecificCall <@@ (==>) @@> (None,_,l::r::[]) -> Some (l,r)
        | _ -> None

    let (|Conseq|_|) =
        function
        | SpecificCall <@@ (<==) @@> (None,_,l::r::[]) -> Some (l,r)
        | _ -> None

    let (|Add|_|) =
        function
        | SpecificCall <@@ (+) @@> (None,_,l::r::[]) -> Some(l, r)
        | _ -> None

    let (|Subtract|_|) =
        function
        | SpecificCall <@@ (-) @@> (None,_,l::r::[]) -> Some(l, r)
        | _ -> None
    
    let (|Multiply|_|) =
        function
        | SpecificCall <@@ (*) @@> (None,_,l::r::[]) -> Some(l, r)
        | _ -> None
        
    let (|Range|_|) =
        function
        | SpecificCall <@@ (..) @@> (None,_,l::r::[]) -> Some(l,r)
        | _ -> None

    let (|Sequence|_|) =
        function
        | Call(None, method, Range(l, r)::[]) when method.Name = "CreateSequence" -> Some (l, r)
        | _ -> None

    let (|Sum|_|)  =
        function
        | Call(None, method, Sequence(l, r)::[]) when method.Name = "Sum" -> Some (l, r)
        | _ -> None

    let (|Unary|_|) (op:Expr<'t->'t>)=
        function
        | SpecificCall op (None,_,r::[]) when r.Type = typeof<'t> -> Some r
        | _ -> None

    let (|Binary|_|) (op:Expr<'t->'t->'t>) =
        function
        | SpecificCall op (None,_,l::r::[]) when l.Type = typeof<'t> && r.Type = typeof<'t> -> Some (l,r)
        | _ -> None

    /// (x + y) + z = x + (y + z)
    let (|Assoc|_|) (op:Expr<'t->'t->'t>) =
        function
        | Binary op (Binary op (a1, a2), a3), Binary op (b1, Binary op (b2, b3)) when sequal3 a1 a2 a3 b1 b2 b3 -> 
            Some (pattern_desc "Associativity" <@ fun x y z -> (%op) ((%op) x y) z = (%op) x ((%op) y z) @>)
        | _ -> None

    /// x + y == y + x
    let (|Commute|_|) (op: Expr<'t->'t->'t>) =
        function
        | Binary op (a1, a2), Binary op (b1, b2) when sequal2 a1 a2 b2 b1 -> Some (pattern_desc "Commutativity" <@fun (x:'t) (y:'t) -> (%op) x y = (%op) y x @>)   
        | _ -> None

    /// x + 0 == x
    let (|OpIdentity|_|) (op: Expr<'t->'t->'t>) (zero:Expr<'t>)   = 
        function
        | Binary op (a1, z), a2 when sequal a1 a2 && sequal zero z -> Some (pattern_desc "Identity" <@ fun (x:'t) -> (%op) x (%zero) = (%zero) @>)
        | _ -> None

    let (|Identity|_|) (a:Expr<'t>) (b:Expr<'t>) = 
        function
        | (a1, b1) when sequal2 a1 b1 a b -> Some (pattern_desc "Equality" <@ %a = %b @>)
        | _ -> None

    /// x * (y + z) == x * y + x * z
    let (|Distrib|_|) (op1: Expr<'t->'t->'t>) (op2: Expr<'t->'t->'t>)  = 
        function
        | Binary op1 (a3, Binary op2 (b3, b4)), Binary op2 (Binary op1 (a1, b1), Binary op1 (a2, b2)) when (sequal a1 a2) && (sequal a1 a3) && sequal2 b1 b2 b3 b4 -> 
                Some (pattern_desc "Distributive" <@ fun x y z -> (%op1) x ((%op2) y z) = (%op2) ((%op1) x y) ((%op1) x z) @>)
        | _ -> None

    ///  -(y + z) == -x  * - z
    let (|UnaryDistrib|_|) (op1: Expr<'t->'t>) (op2: Expr<'t->'t->'t>)  = 
        function
        | Unary op1 (Binary op2 (a1, a2)), Binary op2 (Unary op1 a3, Unary op1 a4) when sequal a1 a3 && sequal a2 a4 -> 
                Some (pattern_desc "Distributive" <@ fun x y -> (%op1) ((%op2) x y) = (%op2) ((%op1) x ) ((%op1) y) @>)
        | _ -> None

    /// x + (-x) == zero
    let (|Inverse|_|) (op: Expr<'t->'t->'t>) (inverse: Expr<'t -> 't>) (zero: Expr<'t>)   =
        function
        | Binary op (a1, Unary inverse (a2)), z  when sequal a1 a2 && sequal zero z -> Some (pattern_desc "Inverse" <@ fun (x:'t) -> (%op) x ((%inverse) x) = (%zero) @>)
        | _ -> None

    /// x + x == x
    let (|Idempotency|_|) (op: Expr<'t->'t->'t>) = 
        function
        | Binary op (a1, a2), a3 when sequal a1 a2 && sequal a1 a3 -> Some (pattern_desc "Idempotency" <@ fun (x:'t) -> ((%op) x x) = x @>)
        | _ -> None

    /// not (x = y) == not(x) <> not(y)
    let (|Duality|_|) (op1: Expr<'t->'t->'t>) (op2: Expr<'t->'t->'t>) (inverse: Expr<'t->'t>) =
        function
        | Unary inverse (Binary op1 (a1, a2)), Binary op2 (Unary inverse b1, Unary inverse b2) when sequal2 a1 a2 b2 b1 -> 
            Some (pattern_desc "Duality" <@ fun x y -> (%inverse)((%op1) x y) = (%op2) ((%inverse)(x)) ((%inverse)(y)) @>)
        | _ -> None

    let (|Defn|_|) (l:Expr<'t>) (r:Expr<'t>) =
        function
        | a1, a2 when sequal2 a1 a2 l r -> Some (pattern_desc "Definition" <@ l = r @>)
        | _ -> None

    let (|BinaryOpDefn|_|) (op1:Expr<'t->'t->'t>) (op2:Expr<'t->'t->'t>) (op3:Expr<'t->'t>)=
        function
        | Binary op1 (a1, a2), Unary op3 (Binary op2 (a3, a4)) when sequal2 a1 a2 a3 a4 -> Some (pattern_desc "Definition" <@ fun x y -> (%op1) x y = (%op2) x y @>)
        | _ -> None

[<AutoOpen>]
module Formula =  
   let True =  <@ true @>
   let False = <@ false @>

   // Make Formula an alias for the reflected definition attribute
   type Formula = ReflectedDefinitionAttribute
   
   // Methods for creating variable names for formulas
   
   let var<'t> = Unchecked.defaultof<'t>
   let var2<'t> = Unchecked.defaultof<'t>, Unchecked.defaultof<'t>
   let var3<'t> = Unchecked.defaultof<'t>, Unchecked.defaultof<'t>, Unchecked.defaultof<'t>
   let var4<'t> = Unchecked.defaultof<'t>, Unchecked.defaultof<'t>, Unchecked.defaultof<'t>, Unchecked.defaultof<'t>
