namespace Sylvester

open System
open System.Collections
open System.Collections.Generic
open System.Linq
   
open Sylvester.Arithmetic
open Sylvester.Collections

/// A set of elements each with type or class denoted by t.
[<CustomEquality; CustomComparison>]
type Set<'t when 't: equality> =
/// The empty set.
| Empty
/// A set defined by the distinct elements of a sequence i.e. a set that has a function from N -> t.
| Seq of seq<'t>
/// A set of elements defined by a set comprehension.
| Set of SetComprehension<'t>
with 
    interface IEnumerable<'t> with
        member x.GetEnumerator () = 
            match x with
            |Empty -> Seq.empty.GetEnumerator()
            |Seq s -> let distinct = s |> Seq.distinct in distinct.GetEnumerator()
            |Set _ -> failwith "Cannot enumerate a set defined by a set comprehension. Use a sequence instead."
                
    interface IEnumerable with
        member x.GetEnumerator () = (x :> IEnumerable<'t>).GetEnumerator () :> IEnumerator

    interface IEquatable<Set<'t>> with
        member a.Equals b =
            match a, b with
            | Empty, Empty -> true
            | _, Empty -> false
            |Empty, _ -> false
            |Generator g1, Generator g2 -> (g1.Seq.ToString() = g2.Seq.ToString()) 
            |Set expr1, Set expr2 ->  expr1 = expr2
            |Seq s1, Seq s2 ->  (Seq.length s1 = Seq.length s2) && s1 |> Seq.forall (fun x -> s2.Contains x) && s2 |> Seq.forall (fun x -> s1.Contains x)
            
            |_,_ -> failwith "Cannot test a sequence and a set defined using a set comprehension for equality. Use 2 finite sequences or 2 set comprehensions."
    
    override a.Equals (_b:obj) = 
            match _b with 
            | :? Set<'t> as b -> (a :> IEquatable<Set<'t>>).Equals b
            | _ -> false
    
    override a.GetHashCode() = 
        match a with
        | Empty -> "Empty".GetHashCode()
        | Generator g -> let h = g.Seq.ToString() + g.Seq.ToString() in h.GetHashCode()
        | Seq s -> s.GetHashCode()
        | Set p -> p.ToString().GetHashCode()

    /// Create a subset of the set using a predicate.
    member x.Subset(f: 't->bool) = 
        match x with
        |Empty -> failwith "The empty set has no subsets."
        |Generator g -> Seq(SetGenerator(x |> Seq.filter f, (fun x -> g.Test(x) && f(x))))
        |Seq _ -> Seq(x |> Seq.filter f) 
        |Set s -> SetComprehension(s.RangeTest, s.Body, (fun x -> s.Test(x) && f x)) |> Set

    /// Determine if the set contains an element.
    member x.HasElement elem = 
        match x, elem with
        |Empty, _ -> false
        |Generator g, e -> g.HasElement e
        |Seq set, e -> set.Contains e 
        |Set s, e -> s.Test e
    
    /// Indicator function for an element.
    member x.Indicate elem = if x.HasElement elem then 1 else 0

    /// Determine if the set contains another set as a subset.
    member a.HasSubset b =
        match a, b with
        | Empty, _ -> false
        | _, Empty -> true
        | _, Seq _ ->  b |> Seq.forall (fun x -> a.HasElement x)
        |Seq _, Set _ ->  failwith "Cannot test if a sequence contains a set defined by a set comprehension as a subset. Use 2 finite sequences or a set builder with a finite sequence."
        |Set _, Set _ ->  failwith "Cannot test two sets defined by a set comprehension for the subset relation. Use 2 finite sequences or a set comprehension with a finite sequence."

    interface IComparable<Set<'t>> with
        member a.CompareTo b = if a = b then 0 else if b.HasSubset a then -1 else if a.HasSubset b then 1 else 0

    interface IComparable with
        member a.CompareTo b = 
            match b with
            | :? Set<'t> as set -> (a :> IComparable<Set<'t>>).CompareTo set
            | _ -> failwith "This object is not a set."

    member a.Difference b =
        match a, b with
        | _, Empty -> a
        | Empty, _ -> Empty
        | _,_ -> a.Subset(fun x -> b.HasElement x |> not)
        
    member a.Difference b =
        match a with
        | Empty -> Empty
        | Generator g -> SetGenerator(a |> Seq.except [b], (fun x -> g.Test(x) && not(x = b))) |> Set.ofGen
        | Seq _ -> Seq(a |> Seq.except [b])
        | Set builder -> SetComprehension(builder.RangeTest, builder.Body, (fun x -> builder.Test(x) && not(x = b))) |> Set
        
    member a.Complement (b:Set<'t>) = b.Difference a
    
    /// Number of elements in the set. 
    member x.Length =
       match x with
       | Empty -> 0
       | Seq _ -> x |> Seq.length
       | _ -> failwith "Cannot get length of a set defined by a set comprehension. Use a finite sequence instead."

    /// Set of all subsets.
    member a.Powerset =
        match a with
        | Empty -> Empty
        | Seq _ ->
                // From http://www.fssnip.net/ff/title/Sequence-of-all-Subsets-of-a-set by Isaiah Permulla
                // using bit pattern to generate subsets
                let max_bits x = 
                    let rec loop acc = if (1 <<< acc ) > x then acc else loop (acc + 1)
                    loop 0
            
                let bit_setAt i x = ((1 <<< i) &&& x) <> 0
                let subsets = 
                        let len = a.Length
                        let as_set x =  seq {for i in 0 .. (max_bits x) do 
                                                if (bit_setAt i x) && (i < len) then yield Seq.item i a}
                        seq {for i in 0 .. (1 <<< len)-1 do yield let s = as_set i in if Seq.isEmpty s then Empty else Seq(s)}
                
                Seq (Gen(subsets, (fun x -> a.HasSubset x)))   
        | _ -> failwith "Cannot enumerate the power set of a set defined by a set comprehension. Use a sequence instead."

    member x.ToSubsets() =
        match x with
        | Empty -> failwith "The empty set has no subsets."
        | Seq s -> s |> Seq.map(fun s -> Seq [s]) |> Seq
        | Set _ -> failwith "Cannot enumerate all subsets of a set defined by a set comprehension."

    member x.Product = 
        match x with
        |Empty -> Empty
        |Seq s -> Seq(Gen(cart s, (fun (a,b) -> x.HasElement a && x.HasElement b)))
        |Set builder -> let t1 = builder.Body in SetComprehension((fun(a, b) -> builder.RangeTest a && builder.RangeTest b), <@ %t1, %t1 @>, (fun (a, b) -> x.HasElement a && x.HasElement b)) |> Set 
    
    static member fromSeq(s: seq<'t>) = Seq s
    
    static member ofGen(gen:Gen<'t>) = Seq gen

    static member ofSubsets(s:seq<'t>) =
        let set = 
            match s with
            | FiniteSeq -> Seq(s |> Seq.distinct |> Seq.toArray)
            | NonFiniteSeq -> Seq(s |> Seq.distinct |> Seq.toArray)
        set.Powerset

    static member toProduct(s:Set<'t>) = s.Product
 
    /// Set union operator.
    [<Symbol "\u222A">]
    static member (|+|) (l, r) = 
        match (l, r) with
        |(Empty, x) -> x
        |(x, Empty) -> x
        |(Seq _, Seq _) -> SetGenerator(Seq.concat[l; r], (fun x -> l.HasElement x || r.HasElement x)) |> Set.ofGen
        |_, _ -> 
            let set_union(l:Set<'t>, r: Set<'t>) = Unchecked.defaultof<'t>
            SetComprehension(<@ set_union(l, r) @>, (fun x -> l.HasElement x || r.HasElement x)) |> Set
        
    /// Set intersection operator.
    [<Symbol "\u2229">]
    static member (|*|) (l, r) = 
        match (l, r) with
        |(Empty, _) -> Empty
        |(_, Empty) -> Empty
        
        |(Seq a, Finite b) -> 
                let s = 
                    Seq.concat [a;b] 
                    |> Seq.filter (fun x -> l.HasElement x && r.HasElement x) 
                    |> Seq.take (b.Count()) 
                Gen(s, (fun x -> l.HasElement x || r.HasElement x)) |> Set.ofGen
        |(Finite a, Seq b) -> 
                let s = 
                    Seq.concat [a;b] 
                    |> Seq.filter (fun x -> l.HasElement x && r.HasElement x) 
                    |> Seq.take (a.Count()) 
                Gen(s, (fun x -> l.HasElement x || r.HasElement x)) |> Set.ofGen
        |(Seq a, Seq b) -> SetGenerator(a.Intersect b, (fun x -> l.HasElement x || r.HasElement x)) |> Set.ofGen
        |(_, _) -> 
            let set_intersect(l:Set<'t>, r: Set<'t>) = Unchecked.defaultof<'t>
            SetComprehension(<@ set_intersect(l,r) @>, (fun x -> l.HasElement x && r.HasElement x)) |> Set

    ///Set 'is element of' operator
    static member (|?|)(e:'t, l:Set<'t>) = l.HasElement e

    /// Set 'is subset of' operator.
    static member (|<|) (l:Set<'t>, r:Set<'t>) = r.HasSubset l

    /// Set relative difference operator: A |^| B = A \ B.
    static member (|^|) (l:Set<'t>, r:Set<'t>) = l.Difference r

    /// Set relative complement operator: A |/| B = B \ A.
    static member (|/|) (l:Set<'t>, r:Set<'t>) = l.Complement r

    /// Set create subset operator.
    static member (|>|) (l:Set<'t>, r:'t->bool) = l.Subset r

    /// Set filter subsets operator.
    static member (|>>|) (l:Set<'t>, r:Set<'t> -> bool) = l.Powerset.Subset r

    /// Set difference operator
    static member (|-|) (l:Set<'t>, r:Set<'t>) = l.Difference r

    /// Set element-wise difference operator
    static member (|-|) (l:Set<'t>, r:'t) = l.Difference r

    /// Set Cartesian product.
    static member (*) (l, r) = 
        match (l, r) with
        |(_, Empty) -> Empty
        |(Empty, _) -> Empty

        |(Seq a, Seq b) -> Seq(Gen(cart2 a b, (fun (x,y) -> l.HasElement x && r.HasElement y)))
        |(_,_) -> SetComprehension(<@ (l.First(), r.First())@>, (fun (x,y) -> l.HasElement x && r.HasElement y)) |> Set
    
    interface ISet<'t> with member x.Set = x

and ISet<'t when 't: equality> = 
    inherit IEquatable<Set<'t>>
    abstract member Set:Set<'t>

and FiniteSet<'n, 't when 'n :> Number and 't : equality>(items: 't[]) =
    member val Length = number<'n>
    member val Items = Array<'n, 't>(items)
    member val Set = Seq items
    interface ISet<'t> with
        member x.Set = x.Set
        member x.Equals y = x.Set.Equals y
    interface Generic.IEnumerable<'t> with
        member x.GetEnumerator():Generic.IEnumerator<'t> = (x.Set :> IEnumerable<'t>).GetEnumerator()
        member x.GetEnumerator():IEnumerator = (x.Set :> IEnumerable).GetEnumerator()
    new (items:seq<'t>) = FiniteSet(items |> Seq.toArray)

and Singleton<'t when 't: equality>(e:'t) = inherit FiniteSet<N<1>, 't>([|e|])

and Family<'t when 't : equality> = Set<'t> list

[<AutoOpen>]
module Set =
    /// Set union operator.
    [<Symbol "\u222A">]
    let (|+|) (l:ISet<'t>) (r:ISet<'t>) = l.Set |+| r.Set
    
    /// Set intersection operator.
    [<Symbol "\u2229">]
    let (|*|) (l:ISet<'t>) (r:ISet<'t>) = l.Set |*| r.Set

    /// Set element of operator
    let (|?|) (e:'t) (l:ISet<'t>) = l.Set.HasElement e

    /// Set subset relation.
    let (|<|) (l:ISet<'t>) (r:ISet<'t>) = l.Set |<| r.Set

    /// Set create subset
    let (|>|) (l:ISet<'t>) (r:'t -> bool) = l.Set |>| r
    
    /// Set filter subset
    let (|>>|) (l:ISet<'t>) (r:Set<'t> -> bool) = l.Set.Powerset |>| r
    
    /// Set difference operator.
    let (|-|) (l:ISet<'t>) (r:ISet<'t>) = l.Set.Difference r.Set

    /// Set-element difference operator.
    let (|^|) (l:ISet<'t>) (r:'t) = l.Set.Difference r

    /// Set complement operator.
    let (|/|) (l:ISet<'t>) (r:ISet<'t>) = l.Set.Complement r.Set
    
    let infiniteSeq g = g |> Seq.initInfinite |> Set.fromSeq

    let infiniteSeq2 g = g |> Seq.initInfinite |> Seq.pairwise |> Set.fromSeq

    let infiniteSeq3 g = g |> Seq.initInfinite |> triplewise |> Set.fromSeq

    let infiniteSeq4 g = g |> Seq.initInfinite |> quadwise |> Set.fromSeq

    let infiniteSeq5 g = g |> Seq.initInfinite |> quintwise |> Set.fromSeq

    let ofType<'t when 't: equality> = fun (_:'t) -> true
    
    /// A singleton set containing 0. 
    let Zero = FiniteSet<N<1>, int>([|0|])

    /// The universal set.
    let U<'t when 't : equality> = SetComprehension (<@ Unchecked.defaultof<'t> @>, (fun (_:'t) -> true)) |> Set