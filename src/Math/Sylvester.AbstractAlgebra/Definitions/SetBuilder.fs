namespace Sylvester

open System
open System.Collections
open System.Collections.Generic
open System.Linq
open FSharp.Quotations
open FSharp.Quotations.Patterns

type Test<'t> = 't -> bool

/// A logical predicate tests if an expression or condition is true or false.
type ILogicalPredicate<'t when 't: equality> = 
    abstract Test:Test<'t>
    abstract Expr:Expr<'t -> bool>

/// A logical predicate tests if an expression or condition is true or false.
type LogicalPredicate<'t when 't: equality>([<ReflectedDefinition(true)>] test:Expr<'t -> bool>) = 
    let v = match test with | WithValue(v, _, _) -> v | _ -> failwith "Unexpected expression."
    member val Test = v :?> ('t -> bool)
    member val Expr = test
    override x.ToString() = x.Expr |> src
    interface ILogicalPredicate<'t> with
        member val Test = v :?> ('t -> bool)
        member val Expr = test
    interface IEquatable<LogicalPredicate<'t>> with member a.Equals(b) = a.ToString() = b.ToString()
    override a.Equals (_b:obj) = 
            match _b with 
            | :? LogicalPredicate<'t> as b -> (a :> IEquatable<LogicalPredicate<'t>>).Equals b
            | _ -> false
    override a.GetHashCode() = (a.ToString()).GetHashCode() 

/// A logical statement that can define a set using a predicate for set membership.
type ISetBuilder<'t when 't: equality> = ILogicalPredicate<'t>
 
/// A statement that defines a set using a predicate for set membership.
type SetBuilder<'t when 't : equality> = LogicalPredicate<'t>

/// A generator defines a sequence together with a logical predicate that tests for set membership. 
type SetGenerator<'t when 't: equality>([<ReflectedDefinition(true)>] test:Expr<'t -> bool>, s:seq<'t>) = 
    let pv = match test with | WithValue(v, _, _) -> v | _ -> failwith "Unexpected expression."
    member val Test = pv :?> ('t -> bool)
    member val Expr = test
    member x.HasElement elem = x.Test elem
    interface IEnumerable<'t> with
        member x.GetEnumerator () = s.GetEnumerator() 
    interface IEnumerable with
        member x.GetEnumerator () = (x :> IEnumerable<'t>).GetEnumerator () :> IEnumerator
    interface ISetBuilder<'t> with
        member val Test = pv :?> ('t -> bool)
        member val Expr = test
     
/// A sequence generating function.
and GeneratingFunction<'t when 't: equality> = int -> 't

/// A logical predicate expression.
type Pred<'t when 't: equality> = LogicalPredicate<'t>    

/// A generator defines a sequence together with a logical predicate that tests for set membership.
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
            let prev = new System.Collections.Generic.List<'a ref>()
           
            while e.MoveNext() do
                let i = ref e.Current
                prev.Add i
                yield! seq {for p in prev do yield (!i, !p)}
                yield! seq {for p in prev do yield (!p, !i)}
                 
        }

    let cart2 (source1: seq<'a>) (source2:seq<'b>) =
        seq { 
            use e1 = source1.GetEnumerator()
            use e2 = source2.GetEnumerator()
            let prev = new System.Collections.Generic.List<'a ref *'b ref>()
           
            while (e1.MoveNext() && e2.MoveNext()) do
                let i = ref e1.Current
                let j = ref e2.Current
                prev.Add ((i, j))
                yield! seq {for p in prev do yield (!i, !(snd p))}
                yield! seq {for p in prev do yield (!(fst p), !j)} 
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