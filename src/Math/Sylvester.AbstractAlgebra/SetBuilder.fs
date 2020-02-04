namespace Sylvester

open System.Collections
open System.Collections.Generic
open System.Linq
open FSharp.Quotations
open FSharp.Quotations.Patterns

/// A logical predicate.
type Predicate<'t when 't: equality> = 't -> bool

//// A logical predicate expression.
type IPredicateExpr<'t when 't: equality> = 
    abstract member Pred:Predicate<'t>
    abstract member Expr:Expr

/// A statement that defines a set using a predicate for set membership.
type ISetBuilder<'t when 't: equality> = IPredicateExpr<'t>

//// A logical predicate expression
type PredicateExpr<'t when 't: equality>([<ReflectedDefinition(true)>] pred:Expr<Predicate<'t>>) = 
    let v,t,e = match pred with | WithValue(v, t, e) -> v,t,e | _ -> failwith "Unexpected expression."
    member val Pred = v :?> Predicate<'t>
    member val Expr = e
    interface IPredicateExpr<'t> with
        member val Pred = v :?> Predicate<'t>
        member val Expr = e

/// A statement that defines a set using a predicate for set membership.
type SetBuilder<'t when 't : equality> = PredicateExpr<'t>

/// A  generating function that defines a sequence together with a logical predicate that tests for set membership. 
type SetGenerator<'t when 't: equality>([<ReflectedDefinition(true)>] pred:Expr<Predicate<'t>>, [<ReflectedDefinition(true)>] gen:Expr<GeneratingFunction<'t>>) = 
    let pv,pt,pe = match pred with | WithValue(v, t, e) -> v,t,e | _ -> failwith "Unexpected expression."
    let gv,gt,ge = match gen with | WithValue(v, t, e) -> v,t,e | _ -> failwith "Unexpected expression."
    let gen = gv :?> GeneratingFunction<'t>
    member val Pred = pv :?> Predicate<'t>
    member val PredExpr = pe
    member val Gen = gen
    member val GenExpr = ge
    member val Seq = Seq.initInfinite gen
    interface IEnumerable<'t> with
        member x.GetEnumerator () = x.Seq.GetEnumerator() 
    interface IEnumerable with
        member x.GetEnumerator () = (x :> IEnumerable<'t>).GetEnumerator () :> IEnumerator
    interface ISetBuilder<'t> with
        member val Pred = pv :?> Predicate<'t>
        member val Expr = pe
    
/// A sequence generating function.
and GeneratingFunction<'t when 't: equality> = int -> 't

type Pred<'t when 't: equality> = PredicateExpr<'t>    
type Gen<'t when 't: equality> = SetGenerator<'t>

[<AutoOpen>]
module SetBuilder =
    let (|Generator|_|) x =
        match x:IEnumerable<'t> with
        | _ when x.GetType().Name.StartsWith("SetGenerator") -> Some (x :?> SetGenerator<'t>)
        | _ -> None

    let (|ArraySeq|ListSeq|SetSeq|GeneratorSeq|OtherSeq|) s =
        match s:IEnumerable<'t> with
        | :? array<'t> -> ArraySeq
        | :? list<'t> ->  ListSeq
        | _ when s.GetType().Name.EndsWith("Set") -> SetSeq
        | _ when s.GetType().Name.StartsWith("SetGenerator") -> GeneratorSeq
        | _ -> OtherSeq

    let (|FiniteContainer|_|) x =
        match x:IEnumerable<'t> with
        | ArraySeq
        | ListSeq
        | SetSeq -> Some x
        | _ -> None
