namespace Sylvester

open System
open System.Collections
open System.Collections.Generic
open System.Linq
open FSharp.Quotations
open FSharp.Quotations.Patterns

/// A logical predicate.
type LogicalPredicate<'t when 't: equality> = 't -> bool

//// A logical predicate expression.
type IPredicateExpr<'t when 't: equality> = 
    abstract member Pred:LogicalPredicate<'t>
    abstract member Expr:Expr

/// A logical statement that can define a set using a predicate for set membership.
type ISetBuilder<'t when 't: equality> = IPredicateExpr<'t>

//// A logical predicate expression
type PredicateExpr<'t when 't: equality>([<ReflectedDefinition(true)>] pred:Expr<LogicalPredicate<'t>>) = 
    let v,t,e = match pred with | WithValue(v, t, e) -> v,t,e | _ -> failwith "Unexpected expression."
    member val Pred = v :?> LogicalPredicate<'t>
    member val Expr = e
    interface IPredicateExpr<'t> with
        member val Pred = v :?> LogicalPredicate<'t>
        member val Expr = e
    interface IEquatable<PredicateExpr<'t>> with member a.Equals(b) = exprToString a.Expr = exprToString b.Expr
    override a.Equals (_b:obj) = 
            match _b with 
            | :? PredicateExpr<'t> as b -> (a :> IEquatable<PredicateExpr<'t>>).Equals b
            | _ -> false
    override a.GetHashCode() = (exprToString a.Expr).GetHashCode() 
 
/// A statement that defines a set using a predicate for set membership.
type SetBuilder<'t when 't : equality> = PredicateExpr<'t>

/// A generating function that defines a sequence together with a logical predicate that tests for set membership. 
type SetGenerator<'t when 't: equality>([<ReflectedDefinition(true)>] pred:Expr<LogicalPredicate<'t>>, [<ReflectedDefinition(true)>] gen:Expr<GeneratingFunction<'t>>) = 
    let pv,pt,pe = match pred with | WithValue(v, t, e) -> v,t,e | _ -> failwith "Unexpected expression."
    let gv,gt,ge = match gen with | WithValue(v, t, e) -> v,t,e | _ -> failwith "Unexpected expression."
    let gen = gv :?> GeneratingFunction<'t>
    member val Pred = pv :?> LogicalPredicate<'t>
    member val PredExpr = pe
    member val Gen = gen
    member val GenExpr = ge
    member val Seq = Seq.initInfinite gen
    member x.HasElement elem = x.Pred elem
    interface IEnumerable<'t> with
        member x.GetEnumerator () = x.Seq.GetEnumerator() 
    interface IEnumerable with
        member x.GetEnumerator () = (x :> IEnumerable<'t>).GetEnumerator () :> IEnumerator
    interface ISetBuilder<'t> with
        member val Pred = pv :?> LogicalPredicate<'t>
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
        | _ when s.GetType().Name.StartsWith("FSharpSet") -> SetSeq
        | :? SetGenerator<'t> -> GeneratorSeq
        | _ -> OtherSeq

    let (|Finite|_|) x =
        match x:IEnumerable<'t> with
        | ArraySeq
        | ListSeq
        | SetSeq -> Some x
        | _ -> None

    let (|FiniteSeq|NonFiniteSeq|) s =
        match s:IEnumerable<'t> with
        | Finite _ -> FiniteSeq
        | _ -> NonFiniteSeq
