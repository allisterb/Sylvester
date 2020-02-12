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
    abstract member ExprString:string
/// A logical statement that can define a set using a predicate for set membership.
type ISetBuilder<'t when 't: equality> = IPredicateExpr<'t>

//// A logical predicate expression
type PredicateExpr<'t when 't: equality>([<ReflectedDefinition(true)>] pred:Expr<LogicalPredicate<'t>>) = 
    let v,t,e = match pred with | WithValue(v, t, e) -> v,t,e | _ -> failwith "Unexpected expression."
    member val Pred = v :?> LogicalPredicate<'t>
    member val Expr = e
    member val ExprString = exprToString e
    interface IPredicateExpr<'t> with
        member val Pred = v :?> LogicalPredicate<'t>
        member val Expr = e
        member val ExprString = exprToString e
    interface IEquatable<PredicateExpr<'t>> with member a.Equals(b) = a.ExprString = b.ExprString
    override a.Equals (_b:obj) = 
            match _b with 
            | :? PredicateExpr<'t> as b -> (a :> IEquatable<PredicateExpr<'t>>).Equals b
            | _ -> false
    override a.GetHashCode() = (exprToString a.Expr).GetHashCode() 
 
/// A statement that defines a set using a predicate for set membership.
type SetBuilder<'t when 't : equality> = PredicateExpr<'t>

/// A generator defines a sequence together with a logical predicate that tests for set membership. 
type SetGenerator<'t when 't: equality>([<ReflectedDefinition(true)>] pred:Expr<LogicalPredicate<'t>>, s:seq<'t>) = 
    let pv,pt,pe = match pred with | WithValue(v, t, e) -> v,t,e | _ -> failwith "Unexpected expression."
    member val Pred = pv :?> LogicalPredicate<'t>
    member val Expr = pe
    member val ExprString = exprToString pe
    ///member val Seq = s
    member x.HasElement elem = x.Pred elem
    interface IEnumerable<'t> with
        member x.GetEnumerator () = s.GetEnumerator() 
    interface IEnumerable with
        member x.GetEnumerator () = (x :> IEnumerable<'t>).GetEnumerator () :> IEnumerator
    interface ISetBuilder<'t> with
        member val Pred = pv :?> LogicalPredicate<'t>
        member val Expr = pe
        member val ExprString = exprToString pe
    
/// A sequence generating function.
and GeneratingFunction<'t when 't: equality> = int -> 't

type Pred<'t when 't: equality> = PredicateExpr<'t>    
type Gen<'t when 't: equality> = SetGenerator<'t>

[<AutoOpen>]
module SetBuilder =
    let (|Generator|_|) x =
        match x:IEnumerable<'t> with
        | :? SetGenerator<'t> -> Some (x :?> SetGenerator<'t>)
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

    let cart (source: seq<'a>) =
        seq { 
            use e = source.GetEnumerator() 
            let prev = new System.Collections.Generic.List<'a>()
           
            while e.MoveNext() do
                let i = ref e.Current
                prev.Add !i
                yield! seq {for p in prev do yield (!i, p)}
                 
        }

    let cart2 (source1: seq<'a>) (source2:seq<'b>) =
        seq { 
            use e1 = source1.GetEnumerator()
            use e2 = source2.GetEnumerator()
            let prev = new System.Collections.Generic.List<'a *'b>()
           
            while (e1.MoveNext() && e2.MoveNext()) do
                let i = ref e1.Current
                let j = ref e2.Current
                prev.Add ((!i, !j))
                yield! seq {for p in prev do yield (!i, snd p)}
                yield! seq {for p in prev do yield (fst p, !j)} 
        }


    // n-wise functions based on http://fssnip.net/50 by ptan

    let triplewise (source: seq<_>) =
        seq { 
            use e = source.GetEnumerator() 
            if e.MoveNext() then
                let i = ref e.Current
                if e.MoveNext() then
                    let j = ref e.Current
                    while e.MoveNext() do
                        let k = e.Current 
                        yield (!i, !j, k)
                        i := !j
                        j := k 
        }

    let quadwise (source: seq<_>) =
        seq { 
            use e = source.GetEnumerator() 
            if e.MoveNext() then
                let i = ref e.Current
                if e.MoveNext() then
                    let j = ref e.Current
                    if e.MoveNext() then
                        let k = ref e.Current
                        while e.MoveNext() do
                            let l = e.Current
                            yield (!i, !j, !k, l)
                            i := !j
                            j := !k
                            k := l
            }

    let quintwise (source: seq<_>) =
        seq { 
            use e = source.GetEnumerator() 
            if e.MoveNext() then
                let i = ref e.Current
                if e.MoveNext() then
                    let j = ref e.Current
                    if e.MoveNext() then
                        let k = ref e.Current
                        if e.MoveNext() then
                            let l = ref e.Current
                            while e.MoveNext() do
                                let m = e.Current
                                yield (!i, !j, !k, !l, m)
                                i := !j
                                j := !k
                                k := !l
                                l :=  m
        }




