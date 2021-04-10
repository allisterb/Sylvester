namespace Sylvester 

open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns

open Patterns
open Descriptions

module SetTheory =
    let desc = axiom_desc "Set Theory"
    
    (* Patterns *)
    let (|Set|_|) =
        function
        | Call(None, mi, BoundVars(bound)::range::body::[]) when mi.Name = "set" -> Some(<@@ set @@>, bound, range, body)
        | _ -> None

    let (|ElementOf|_|) =
        function
        | SpecificCall <@@ (|?|) @@> (None,_,l::r::[]) -> Some(l, r)
        | _ -> None

    (* Axioms *)

    /// true = p = p
    let (|True|_|) =
        function
        | Equals(ElementOf(F, Set(_, bound, range, body)), Exists(_, bound', range', body')) -> Some ()
        //| Equals(Bool true, Equals(a1, a2)) when sequal a1 a2 -> pattern_desc "Definition of true" <@fun x -> x = x = true @> |> Some
        | _ -> None    
