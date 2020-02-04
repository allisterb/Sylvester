namespace Sylvester

open System
open System.Collections
open System.Collections.Generic
open System.Linq
    
/// A set of elements each with type or class denoted by t.
[<CustomEquality; NoComparison>]
type Set<'t when 't: equality> =
/// The empty set.
| Empty
/// A set defined by the distinct elements of a sequence i.e. a set that has a function from N -> t.
| Seq of seq<'t>
/// A set of elements defined by a set builder statement.
| Set of SetBuilder<'t>
with 
    interface ISet<'t> with member x.Set = x
    interface IEquatable<Set<'t>> with
        member a.Equals b =
            match a, b with
            | Empty, Empty -> true
            | _, Empty -> false
            |Empty, _ -> false

            |Seq (Finite s1), Seq (Finite s2) ->  a |> Seq.forall (fun x -> b.Contains x) && b |> Seq.forall (fun x -> a.Contains x)
            |Set expr1, Set expr2 ->  expr1.Equals expr2

            |_,_ -> failwith "Cannot test a sequence and a set builder statement for equality. Use 2 finite sequences or 2 set builders."
    override a.Equals (_b:obj) = 
            match _b with 
            | :? Set<'t> as b -> (a :> IEquatable<Set<'t>>).Equals b
            | _ -> false
    override a.GetHashCode() = 
        match a with
        | Empty -> 0
        | Seq s -> s.GetHashCode()
        | Set p -> p.GetHashCode()

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

    /// Create a subset of the set using a predicate.
    member x.Subset(f: LogicalPredicate<'t>) = 
        match x with
        |Empty -> failwith "The empty set has no subsets."
        |Seq s -> Seq(s |> Seq.filter f) 
        |Set s -> SetBuilder(fun x -> s.Pred(x) && f(x)) |> Set

    /// Determine if the set contains an element.
    member x.HasElement(elem: 't) = 
        match x with
        |Empty -> false
        |Seq s -> 
            match s with
            | Generator g -> g.HasElement elem
            | _ -> s.Contains elem // May fail
        |Set s -> s.Pred elem
    
    member a.HasSubset b =
        match a, b with
        | Empty, Empty -> false
        | _, Empty -> true
        |Empty, _ -> false

        |Seq (Finite s1), Seq (Finite s2) ->  b |> Seq.forall (fun x -> a.Contains x)
        |Set s1, Seq (Finite s2) ->  b |> Seq.forall (fun x -> a.HasElement x)
        |Seq s1, Set s2 ->  failwith "Cannot test if a sequence contains a set builder statement as a subset. Use 2 finite sequences or a set builder with a finite sequence."
        |Set expr1, Set expr2 ->  failwith "Cannot test two set builder statements for the subset relation. Use 2 finite sequences or a set builder with a finite sequence."

        |_,_ -> failwith "Cannot test a non for the subset relation. Use 2 finite sequences or a set builder with a finite sequence."

    member x.Length =
       match x with
       | Empty -> 0
       | Seq s ->
            match s with
            | Finite c -> c |> Seq.distinct |> Seq.length 
            | _ -> failwith "Cannot get length of an arbitrary sequence. Use a finite sequence instead."
       | _ -> failwith "Cannot get length of a set defined by a set builder statement. Use a finite sequence instead."

    member x.Subsets =
        match x with
        | Empty -> failwith "The empty set has no subsets."
        | Seq s ->
            match s with
            | Finite c ->
                //using bit pattern to generate subsets
                let max_bits x = 
                    let rec loop acc = if (1 <<< acc ) > x then acc else loop (acc + 1)
                    loop 0
                
                let bit_setAt i x = ((1 <<< i) &&& x) <> 0
                let subsets = 
                        
                        let len = (Seq.length c)
                        let as_set x =  seq {for i in 0 .. (max_bits x) do 
                                                if (bit_setAt i x) && (i < len) then yield Seq.item i c}
          
                        Seq(seq{for i in 0 .. (1 <<< len)-1 -> let s = as_set i in if Seq.length(s) = 0 then Empty else Seq(s |> Seq.toArray)} |> Seq.toArray)
                subsets
            | _ -> failwith "Cannot get all subsets of an arbitrary sequence. Use a finite sequence instead."
        | _ -> failwith "Cannot get all subsets of a set edfined by a set builder statement. Use a finite sequence instead."
        
    static member ofSeq(s:seq<'t>) = Seq(s)

    static member ofSubsets(s:seq<'t>) = 
        let set = 
            match s with
            | FiniteSeq -> Seq(s |> Seq.toArray)
            | NonFiniteSeq -> Seq(s |> Seq.toArray)
        set.Subsets
 
    /// Set union operator.
    static member (|+|) (l, r) = 
        match (l, r) with
        |(Empty, x) -> x
        |(x, Empty) -> x
        |(a, b) -> SetBuilder(fun x -> l.HasElement x || r.HasElement x) |> Set
        
    /// Set intersection operator.
    static member (|*|) (l, r) = 
        match (l, r) with
        |(Empty, _) -> Empty
        |(_, Empty) -> Empty
        |(a, b) -> SetBuilder(fun x -> l.HasElement x && r.HasElement x) |> Set

    /// Set membership operator.
    static member (|<|) (elem:'t, set:Set<'t>) = set.HasElement elem

    /// Set Cartesian product.
    static member (*) (l, r) = 
        match (l, r) with
        |(Empty, Empty) -> Empty
        |(a, Empty) -> SetBuilder(fun (x, y) -> l.HasElement x) |> Set
        |(Empty, b) -> SetBuilder(fun (x, y) -> r.HasElement y) |> Set
        |(a, b) -> SetBuilder(fun (x, y) -> l.HasElement x && r.HasElement y) |> Set
        
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

    let infiniteSeq g = g |> Seq.initInfinite |> Seq  

    let infiniteSeq2 g = g |> infiniteSeq |> Seq.pairwise |> Seq

    let infiniteSeq3 g = 
        g |> infiniteSeq |> triplewise |> Seq

    let infiniteSeq4 g = 
        g |> infiniteSeq |> quadwise |> Seq

    let infiniteSeq5 g =
        g |> infiniteSeq |> quintwise |> Seq
        
    let infiniteGen g f = Gen(f, g) :> seq<'t> |> Seq