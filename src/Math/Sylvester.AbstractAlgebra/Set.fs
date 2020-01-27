namespace Sylvester

open System.Collections
open System.Collections.Generic
open System.Linq

open Sylvester.Arithmetic
/// A set of elements belonging to a universe denoted by U.
type Set<'U when 'U: equality> =
/// The empty set
| Empty

/// A sequence of elements i.e. a function from N -> U.
| Seq of seq<'U>

/// A set of elements of U defined by a predicate.
| Set of ('U -> bool)
    
with 
    /// Set union operator.
    static member inline (|+|) (l, r) = 
        match (l, r) with
        |(Empty, x) -> x
        |(x, Empty) -> x
        
        |(Seq a, Seq b) -> Seq.concat([a; b]) |> Seq
        |(Set a, Set b) -> Set(fun x -> a (x) || b(x))

        |(Set a, Seq b) -> Set(fun x -> a(x) || Seq.contains x b)
        |(Seq a, Set b) -> Set(fun x -> Seq.contains x a || b(x))

    /// Set intersection operator.
    static member inline (|*|) (l, r) = 
        match (l, r) with
        |(Empty, _) -> Empty
        |(_, Empty) -> Empty
        
        |(Seq a, Seq b) -> a.Intersect(b) |> Seq
        |(Set a, Set b) -> Set(fun x -> a(x) && b(x))

        |(Set a, Seq b) -> Set(fun x -> a(x) && Seq.contains x b)
        |(Seq a, Set b) -> Set(fun x -> Seq.contains x a && b(x))

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

[<AutoOpen>]
module Set =
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

/// The cardinality of a set.
[<RequireQualifiedAccess>]
module Card = 
    /// Cardinality 0.
    type zero = N10.zero

    /// Cardinality 1.
    type one = N10.one

    /// Cardinality 2.
    type two = N10.two

    /// Cardinality 3.
    type three = N10.three

    /// Cardinality 4.
    type four = N10.four

    /// Cardinality 5.
    type five = N10.five

    /// Cardinality 6.
    type six = N10.six

    /// Cardinality 7.
    type seven = N10.seven

    /// Cardinality 8.
    type eight = N10.eight

    /// Cardinality 9.
    type nine = N10.nine

    /// Cardinality 10.
    type ten = N10.ten

    let zero = new zero()

    let one = new one()

    let two = new two()

    let three = new three()

    let four = new four()

    let five = new five()

    let six = new six()

    let seven = new seven()

    let eight = new eight()

    let nine = new nine()

    let ten = new ten()

    