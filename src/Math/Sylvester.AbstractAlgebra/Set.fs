namespace Sylvester

open System.Collections
open System.Collections.Generic
open System.Linq

open Sylvester.Arithmetic

/// A set of elements belonging to a universe denoted by U.
type Set<'U when 'U: equality> =
/// The empty set.
| Empty
/// A sequence of elements i.e. a function from N -> U.
| Seq of seq<'U>
/// A set of elements of U defined by a predicate.
| Set of ('U -> bool)
    
with 
    interface IEnumerable<'U> with
        member x.GetEnumerator () = 
            match x with
            |Empty -> Seq.empty.GetEnumerator()
            |Seq s -> let distinct = Seq.distinct s in distinct.GetEnumerator()
            |Set s -> failwith "Cannot enumerate an arbitrary set. Use a sequence instead."
                
    interface IEnumerable with
        member x.GetEnumerator () = (x :> IEnumerable<'U>).GetEnumerator () :> IEnumerator

    /// A subset of the set.
    member x.Sub(f: 'U -> bool) = 
        match x with
        |Empty -> failwith "The empty set has no subsets."
        |Seq s -> Seq(s |> Seq.filter f)
        |Set s -> Set(fun x -> s(x) && f(x))

    /// A subset of the set.
    member x.Contains(elem: 'U) = 
        match x with
        |Empty -> false
        |Seq s -> elem |> s.Contains
        |Set s -> s elem

    /// Set union operator.
    static member (|+|) (l, r) = 
        match (l, r) with
        |(Empty, x) -> x
        |(x, Empty) -> x
        
        |(Seq a, Seq b) -> Seq.concat([a; b]) |> Seq
        |(Set a, Set b) -> Set(fun x -> a (x) || b(x))

        |(Set a, Seq b) -> Set(fun x -> a(x) || b |> Seq.contains x)
        |(Seq a, Set b) -> Set(fun x -> a |> Seq.contains x || b(x))

    /// Set intersection operator.
    static member (|*|) (l, r) = 
        match (l, r) with
        |(Empty, _) -> Empty
        |(_, Empty) -> Empty
        
        |(Seq a, Seq b) -> a.Intersect(b) |> Seq
        |(Set a, Set b) -> Set(fun x -> a(x) && b(x))

        |(Set a, Seq b) -> Set(fun x -> a(x) && Seq.contains x b)
        |(Seq a, Set b) -> Set(fun x -> Seq.contains x a && b(x))

    /// Set membership operator.
    static member (|<|) (elem:'U, set:Set<'U>) = set.Contains elem

[<AutoOpen>]
module Set =
    let infiniteSeq f = f |> Seq.initInfinite |> Seq  

    let infiniteSeq2 f = f |> infiniteSeq |> Seq.pairwise |> Seq

    /// n-wise functions based on http://fssnip.net/50 by ptan
    
    let infiniteSeq3 f = 
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
        f |> infiniteSeq |> triplewise |> Seq

    let infiniteSeq4 f = 
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
        f |> infiniteSeq |> quadwise |> Seq

    let infiniteSeq5 f = 
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
        f |> infiniteSeq |> quintwise |> Seq    

    let Int = Set(fun (_:int) -> true)
    let IntU = Set(fun (_:uint32) -> true)

    let IntL = Set(fun (_:int64) -> true)
    let IntUL = Set(fun (_:uint64) -> true)

    let Integers = Int