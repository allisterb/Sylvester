namespace Sylvester

open System
open System.Collections
open System.Collections.Generic
open System.Linq
   
open Sylvester.Arithmetic
open Sylvester.Collections

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

            |Generator g1, Generator g2 -> g1.PredExpr = g2.PredExpr
            |Seq _, Seq _ ->  a |> Seq.forall (fun x -> b.HasElement x) && b |> Seq.forall (fun x -> a.HasElement x)
            |Set expr1, Set expr2 ->  expr1 = expr2
            
            |_,_ -> failwith "Cannot test a sequence and a set defined using a set builder statement for equality. Use 2 finite sequences or 2 set builders."
    
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
            |Set s -> failwith "Cannot enumerate a set defined by a set builder statement. Use a sequence instead."
                
    interface IEnumerable with
        member x.GetEnumerator () = (x :> IEnumerable<'t>).GetEnumerator () :> IEnumerator
  
    member x.Builder =
        match x with
        | Empty -> failwith "This set is the empty set."
        | Set builder -> builder
        | Seq s -> match s with | Generator gen -> SetBuilder(gen.Pred) | _ -> failwith "This sequence is not defined by a generating function."
        
    member x.Generator = 
        match x with
        | Seq s -> match s with | Generator gen -> gen | _ -> failwith "This sequence is not defined by a generating function."
        | _ -> failwith "This set is not a sequence."

    /// Create a subset of the set using a predicate.
    member x.Subset(f: LogicalPredicate<'t>) = 
        match x with
        |Empty -> failwith "The empty set has no subsets."
        |Generator g -> Seq(SetGenerator((fun x -> g.Pred(x) && f(x)), g.Seq |> Seq.filter f))
        |Seq s -> Seq(s |> Seq.filter f) 
        |Set s -> SetBuilder(fun x -> s.Pred(x) && f(x)) |> Set

    /// Determine if the set contains an element.
    member x.HasElement(elem: 't) = 
        match x with
        |Empty -> false
        |Generator g -> g.HasElement elem
        |Seq s -> s.Contains elem // May fail if sequence is infinite
        |Set s -> s.Pred elem
    
    member a.HasSubset b =
        match a, b with
        | Empty, Empty -> false
        | _, Empty -> true
        |Empty, _ -> false

        |_, Seq _ ->  b |> Seq.forall (fun x -> a.HasElement x)
        
        |Seq _, Set _ ->  failwith "Cannot test if a sequence contains a set defined by set builder statement as a subset. Use 2 finite sequences or a set builder with a finite sequence."
        |Set _, Set _ ->  failwith "Cannot test two sets defined by set builder statements for the subset relation. Use 2 finite sequences or a set builder with a finite sequence."

    member a.Difference b =
        match a, b with
        | _, Empty -> a
        | Empty, _ -> Empty
        | _,_ -> a.Subset(fun x -> b.HasElement x |> not)
        
    member a.Difference b =
        match a with
        | Empty -> Empty
        | Generator g -> SetGenerator((fun x -> g.Pred(x) && not(x = b)), g.Seq |> Seq.except [b]) |> Set.ofGen
        | Seq s -> Seq(s |> Seq.except [b])
        | Set builder -> SetBuilder(fun x -> builder.Pred(x) && not(x = b)) |> Set
        
    member x.Length =
       match x with
       | Empty -> 0
       | Seq s -> s |> Seq.distinct |> Seq.length
       | _ -> failwith "Cannot get length of a set defined by a set builder statement. Use a finite sequence instead."

    member x.Powerset =
        match x with
        | Empty -> failwith "The empty set has no subsets."
        | Seq c ->
                // From http://www.fssnip.net/ff/title/Sequence-of-all-Subsets-of-a-set by Isaiah Permulla
                // using bit pattern to generate subsets
                let max_bits x = 
                    let rec loop acc = if (1 <<< acc ) > x then acc else loop (acc + 1)
                    loop 0
                
                let bit_setAt i x = ((1 <<< i) &&& x) <> 0
                let subsets = 
                        let len = Seq.length c
                        let as_set x =  seq {for i in 0 .. (max_bits x) do 
                                                if (bit_setAt i x) && (i < len) then yield Seq.item i c}
                        Seq(seq{for i in 0 .. (1 <<< len)-1 -> let s = as_set i in if Seq.length(s) = 0 then Empty else Seq(s |> Seq.toArray)})
                subsets
            
        | _ -> failwith "Cannot get subsets of a set defined by a set builder statement. Use a finite sequence instead."
    
    static member ofGen(gen:Gen<'t>) = Seq gen

    static member ofSubsets(s:seq<'t>) =
        let set = 
            match s with
            | FiniteSeq -> Seq(s |> Seq.toArray)
            | NonFiniteSeq -> Seq(s |> Seq.toArray)
        set.Powerset
 
    /// Set union operator.
    static member (|+|) (l, r) = 
        match (l, r) with
        |(Empty, x) -> x
        |(x, Empty) -> x
        
        |(Seq a, Seq b) -> SetGenerator((fun x -> l.HasElement x || r.HasElement x), Seq.concat[a; b]) |> Set.ofGen
        |(_, _) -> SetBuilder(fun x -> l.HasElement x || r.HasElement x) |> Set
        
    /// Set intersection operator.
    static member (|*|) (l, r) = 
        match (l, r) with
        |(Empty, _) -> Empty
        |(_, Empty) -> Empty
        
        |(Seq a, Finite b) -> 
                    SetGenerator((fun x -> l.HasElement x || r.HasElement x), Seq.concat [a;b] 
                    |> Seq.filter (fun x -> l.HasElement x && r.HasElement x) 
                    |> Seq.take (b.Count())) 
                    |> Set.ofGen
        |(Finite a, Seq b) -> 
                    SetGenerator((fun x -> l.HasElement x || r.HasElement x), Seq.concat [a;b] 
                    |> Seq.filter (fun x -> l.HasElement x && r.HasElement x) 
                    |> Seq.take (a.Count())) 
                    |> Set.ofGen
        |(Seq a, Seq b) -> SetGenerator((fun x -> l.HasElement x || r.HasElement x), a.Intersect b) |> Set.ofGen
        |(_, _) -> SetBuilder(fun x -> l.HasElement x && r.HasElement x) |> Set

    /// Set has subset operator.
    static member (|<|) (l:Set<'t>, r:Set<'t>) = r.HasSubset l

    /// Set difference operator
    static member (|-|) (l:Set<'t>, r:Set<'t>) = l.Difference r

    /// Set element-wise difference operator
    static member (|-|) (l:Set<'t>, r:'t) = l.Difference r

    /// Set relative complement operator: A |/| B = B \ A.
    static member (|/|) (l:Set<'t>, r:Set<'t>) = r.Difference l

    /// Set Cartesian product.
    static member (*) (l, r) = 
        match (l, r) with
        |(_, Empty) -> Empty
        |(Empty, _) -> Empty

        |(Seq a, Seq b) -> Seq(Gen((fun (x,y) -> l.HasElement x && r.HasElement y), Seq.allPairs a b))
        |(_,_) -> SetBuilder(fun (x,y) -> l.HasElement x && r.HasElement y) |> Set
        
and ISet<'t when 't: equality> = abstract member Set:Set<'t>

and FiniteSet<'n, 't when 'n :> Number and 't : equality>(items: 't[]) =
    member val Length = number<'n>
    member val Items = Array<'n, 't>(items)
    member val Set = Seq items
    interface ISet<'t> with
        member x.Set = x.Set
    interface Generic.IEnumerable<'t> with
        member x.GetEnumerator():Generic.IEnumerator<'t> = (x.Set :> IEnumerable<'t>).GetEnumerator()
        member x.GetEnumerator():IEnumerator = (x.Set :> IEnumerable).GetEnumerator()
type Singleton<'t when 't: equality>(e:'t) = inherit FiniteSet<N<1>, 't>([|e|])

[<AutoOpen>]
module Set =
    /// Set union operator.
    let (|+|) (l:ISet<'t>) (r:ISet<'t>) = l.Set |+| r.Set
    
    /// Set intersection operator.
    let (|*|) (l:ISet<'t>) (r:ISet<'t>) = l.Set |*| r.Set

    /// Set subset relation.
    let (|<|) (l:ISet<'t>) (r:ISet<'t>) = l.Set |<| r.Set
    
    /// Set difference operator.
    let (|-|) (l:ISet<'t>) (r:ISet<'t>) = l.Set.Difference r.Set

    /// Set- element difference operator.
    let (|^|) (l:ISet<'t>) (r:'t) = l.Set.Difference r

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

    let infiniteSeq f g = Gen(f, g |> Seq.initInfinite) |> Set.ofGen  

    let infiniteSeq2 f g = Gen(f, g |> Seq.initInfinite |> Seq.pairwise) |> Set.ofGen

    let infiniteSeq3 f g = Gen(f, g |> Seq.initInfinite |> triplewise) |> Set.ofGen

    let infiniteSeq4 f g = Gen(f, g |> Seq.initInfinite |> quadwise) |> Set.ofGen

    let infiniteSeq5 f g = Gen(f, g |> Seq.initInfinite |> quintwise) |> Set.ofGen

    let ofType<'t when 't: equality> = fun (_:'t) -> true
    
    /// A singleton set containing 0. 
    let Zero = FiniteSet<N<1>, int>([|0|])