namespace Sylvester

open System.Collections
open System.Collections.Generic
open System.Linq
    
/// A set of elements each with type or class denoted by t.
type Set<'t when 't: equality> =
/// The empty set.
| Empty
/// A set defined by the (distinct) elements of a sequence i.e. a set that has a function from N -> t.
| Seq of seq<'t>
/// A set of elements defined by a set builder statement.
| Set of SetBuilder<'t>
with 
    interface ISet<'t> with member x.Set = x

    interface IEnumerable<'t> with
        member x.GetEnumerator () = 
            match x with
            |Empty -> Seq.empty.GetEnumerator()
            |Seq s -> s.GetEnumerator()
            |Set s -> failwith "Cannot enumerate an arbitrary set. Use a sequence instead."
                
    interface IEnumerable with
        member x.GetEnumerator () = (x :> IEnumerable<'t>).GetEnumerator () :> IEnumerator
  
    member x.Builder =
        match x with
        | Empty -> failwith "This set is the empty set."
        | Set sb -> sb
        | Seq s -> match s with | Generator gen -> SetBuilder(gen.Pred) | _ -> failwith "This sequence is not defined by a generating function."
        
    member x.Generator = 
        match x with
        | Seq s -> match s with | Generator gen -> gen | _ -> failwith "This sequence is not defined by a generating function."
        | _ -> failwith "This set is not a sequence."

    /// Create a subset of the set.
    member x.Subset(f: 't -> bool) = 
        match x with
        |Empty -> failwith "The empty set has no subsets."
        |Seq s -> Seq(s |> Seq.filter f) 
        |Set s -> Pred(fun x -> s.Pred(x) && f(x)) |> Set

    /// Determine if the set contains an element.
    member x.HasElement(elem: 't) = 
        match x with
        |Empty -> false
        |Seq s -> 
            match s with
            | Generator g -> g.Pred elem
            | ArraySeq -> s.Contains elem
            | ListSeq -> s.Contains elem
            | SetSeq -> s.Contains elem
            | OtherSeq -> failwith "The HasElement function is not defined for a arbitrary sequence. Use a finite sequence type or a set generator."
        |Set s -> s.Pred elem
 
    /// Set union operator.
    static member (|+|) (l, r) = 
        match (l, r) with
        |(Empty, x) -> x
        |(x, Empty) -> x
        
        |(Seq a, Seq b) -> Seq.concat([a; b]) |> Seq
        |(Set a, Set b) -> SetBuilder(fun x -> a.Pred (x) || b.Pred(x)) |> Set

        |(Set a, Seq b) -> SetBuilder(fun x -> a.Pred(x) || b |> Seq.contains x) |> Set
        |(Seq a, Set b) -> SetBuilder(fun x -> a |> Seq.contains x || b.Pred(x)) |> Set

    /// Set intersection operator.
    static member (|*|) (l, r) = 
        match (l, r) with
        |(Empty, _) -> Empty
        |(_, Empty) -> Empty
        
        |(Seq a, Seq b) -> a.Intersect(b) |> Seq
        |(Set a, Set b) -> SetBuilder(fun x -> a.Pred(x) && b.Pred(x)) |> Set

        |(Set a, Seq b) -> SetBuilder(fun x -> a.Pred(x) && Seq.contains x b) |> Set
        |(Seq a, Set b) -> SetBuilder(fun x -> Seq.contains x a && b.Pred(x)) |> Set

    /// Set membership operator.
    static member (|<|) (elem:'t, set:Set<'t>) = set.Contains elem

    /// Set Cartesian product.
    static member (*) (l, r) = 
        match (l, r) with
        |(Empty, Empty) -> Empty
        |(Empty, a) -> Seq.allPairs Seq.empty a |> Seq 
        |(a, Empty) -> Seq.allPairs a Seq.empty |> Seq

        |(Seq a, Seq b) -> Seq.allPairs a b |> Seq
        |(Set a, Set b) -> SetBuilder(fun (x, y) -> a.Pred(x) && b.Pred(y)) |> Set

        |(Set a, Seq b) -> SetBuilder(fun (x, y) -> a.Pred(x) && b |> Seq.contains y) |> Set
        |(Seq a, Set b) -> SetBuilder(fun (x, y) -> b.Pred(y) && a |> Seq.contains x) |> Set

and ISet<'t when 't: equality> = abstract member Set:Set<'t>

[<AutoOpen>]
module Set =
    let (|+|) (l:ISet<'t>) (r:ISet<'t>) = l.Set |+| r.Set
    
    let (|*|) (l:ISet<'t>) (r:ISet<'t>) = l.Set |*| r.Set

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

    let infiniteSeq f = f |> Seq.initInfinite |> Seq  

    let infiniteSeq2 f = f |> infiniteSeq |> Seq.pairwise |> Seq

    let infiniteSeq3 f = 
        f |> infiniteSeq |> triplewise |> Seq

    let infiniteSeq4 f = 
        f |> infiniteSeq |> quadwise |> Seq

    let infiniteSeq5 f =
        f |> infiniteSeq |> quintwise |> Seq        