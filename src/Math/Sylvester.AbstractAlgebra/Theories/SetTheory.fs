namespace Sylvester 

open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns

open Patterns
open Descriptions

open SetAlgebra

type Set'<'t when 't: equality> = Expr<Set<'t>>

/// Theory of sets and set algebra.
module SetTheory =
    let desc = Some << axiom_desc' "Set Theory"
    
    (* Patterns *)
    
    let (|SetEmpty|_|) =
        function
        | NewUnionCase(uc, e) when uc.Name = "Empty" -> e |> List.map expand |> Some
        | _ -> None

    let (|SetSeq|_|) =
        function
        | NewUnionCase(uc, Sequence e::[]) when uc.Name = "Seq" -> Some e
        | Call(None, mi, l) when mi.Name = "infinite_seq" || mi.Name = "finite_seq" || mi.Name = "sseq" -> l |> List.map expand |> Some
        | _ -> None

    let (|SetComp|_|) =
        function
        | Call(None, mi, (BoundVars(_)::s as c)) when mi.Name = "finite_set" || mi.Name = "infinite_set_0" || mi.Name = "infinite_set_1" || mi.Name = "set" || mi.Name = "set'" -> c |> List.map expand |> Some 
        | _ -> None

    let (|Set|_|) =
        function
        | SetEmpty e
        | SetSeq e
        | SetComp e -> Some e
        | _ -> None

    let (|ElementOf|_|) =
        function
        | SpecificCall <@@ (|?|) @@> (None,_,l::Set r::[]) -> Some(l, r)
        | _ -> None

    (* Axioms *)

    /// e |?| set x (x > 1) (x ^^ 2) = exists x (x > 1) (e = x ^^ 2)
    let (|Membership|_|) =
        function
        | Equals(ElementOf(F, BoundVars(bound)::range::body::_), Exists(_, bound', range', Equals(F', body'))) when vequal' bound bound' && sequal3 F range body F' range' body' -> desc "Set Membership"
        | _ -> None    

    
    let (|Extensionality|_|) =
        function
        |Equals(Equals((Set(BoundVars(sbound)::srange::sbody::_) as S), (Set(BoundVars(tbound)::trange::tbody::_) as T)), ForAll(_, stbound, strange, Equals(ElementOf(x, S'), ElementOf(x', T')))) 
            when sequal S T && vequal' sbound stbound && sequal srange strange -> desc "Set Extensionality"
        | _ -> None

    (* Theory *)

    type SetTheory<'t when 't : equality>(?axioms:Axioms, ?rules:Rules) = inherit SetAlgebra<'t>(defaultArg axioms (fun _ -> None), defaultArg rules [])

    let set_theory<'t when 't: equality> = SetTheory<'t>()

    (* Predicates *)
    
    let bounded_set = pred<Set<_>>
