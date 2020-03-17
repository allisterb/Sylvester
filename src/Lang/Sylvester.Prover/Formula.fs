namespace Sylph

open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns

open Sylvester

/// Logical operators for formulas.
[<AutoOpen>]
module Operators =
    let (|&|) (l:bool) (r:bool) = l && r
    let (|||) (l:bool) (r:bool) = l || r
    let (==>) (l:bool) (r:bool) = (not l) || r
    let (<==) (l:bool) (r:bool) = r ==> l

module FormulaPatterns =
    // The (=) operator is logical equivalence which is associative i.e we can say a = b = c.
    // The == operator is conjunctional equality: A == B == C means A == B and A == C.
    // This is the opposite convention to what Gries et.al adopts for E but we must
    // do it this way because of limitations on how we can use the F# (=) operator. 
    let (|Equiv|_|) = 
         function
         | SpecificCall <@@ (=) @@> (None,_,l::r::[]) -> Some(l, r)
         | _ -> None
        
    let (|Conj|_|) =
        function
        | Equiv(expr2), Bool true -> Some expr2
        | _ -> None

    let (|Symm|):(Expr * Expr)->(Expr * Expr) =
        function
        | (A, B) -> (B, A)

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

    let (|UnaryOp|_|) =
        function
        | Call(None, _, l::[]) -> Some l
        | _ -> None

    let (|BinaryOp|_|) =
        function
        | Call(None, _, l::r::[]) -> Some (l, r)
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

[<AutoOpen>]
module Formula =  
   type Formula = ReflectedDefinitionAttribute
   let var<'t> = Unchecked.defaultof<'t>
   let var2<'t> = Unchecked.defaultof<'t>, Unchecked.defaultof<'t>
   let var3<'t> = Unchecked.defaultof<'t>, Unchecked.defaultof<'t>, Unchecked.defaultof<'t>
   let var4<'t> = Unchecked.defaultof<'t>, Unchecked.defaultof<'t>, Unchecked.defaultof<'t>, Unchecked.defaultof<'t>

   let True =  <@ true @>
   let False = <@ false @>