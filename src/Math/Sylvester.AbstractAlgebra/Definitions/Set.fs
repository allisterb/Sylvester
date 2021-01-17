namespace Sylvester

open System
open System.Collections
open System.Collections.Generic
open System.Linq
open FSharp.Quotations

open Sylvester.Arithmetic
open Sylvester.Collections

/// A set of elements each with type or class denoted by t.
[<CustomEquality; CustomComparison>]
type Set<'t when 't: equality> =
/// The empty set.
| Empty
/// A set defined by the distinct elements of a sequence i.e. a set that is the co-domain of a function from N -> 't.
| Seq of seq<'t>
/// A set of elements formally defined by a range and a body expression. 
| Set of SetComprehension<'t>
with 
    interface IEnumerable<'t> with
        member x.GetEnumerator () = 
            match x with
            |Empty -> Seq.empty.GetEnumerator()
            |Seq s -> let distinct = s |> Seq.distinct in distinct.GetEnumerator()
            |Set _ -> failwith "Cannot enumerate the members of a set comprehension. Use a sequence instead."
                
    interface IEnumerable with
        member x.GetEnumerator () = (x :> IEnumerable<'t>).GetEnumerator () :> IEnumerator

    interface IEquatable<Set<'t>> with
        member a.Equals b =
            match a, b with
            | Empty, Empty -> true
            | _, Empty -> false
            | Empty, _ -> false
            | Set expr1, Set expr2 ->  expr1 = expr2
            | Seq s1, Seq s2 ->  (Seq.length s1 = Seq.length s2) && s1 |> Seq.forall (fun x -> s2.Contains x) && s2 |> Seq.forall (fun x -> s1.Contains x)
            
            |_,_ -> failwith "Cannot test a sequence and a set comprehension for equality. Use 2 finite sequences or 2 set comprehensions."
    
    interface IComparable<Set<'t>> with
        member a.CompareTo b = if a = b then 0 else if b.HasSubset a then -1 else if a.HasSubset b then 1 else 0

    interface IComparable with
        member a.CompareTo b = 
            match b with
            | :? Set<'t> as set -> (a :> IComparable<Set<'t>>).CompareTo set
            | _ -> failwith "This object is not a set."

    override a.Equals (_b:obj) = 
            match _b with 
            | :? Set<'t> as b -> (a :> IEquatable<Set<'t>>).Equals b
            | _ -> false
    
    override a.GetHashCode() = 
        match a with
        | Empty -> "Empty".GetHashCode()
        | Seq s -> s.ToString().GetHashCode()
        | Set p -> p.ToString().GetHashCode()

    override x.ToString() =
        match x with
        | Empty -> "Empty"
        | Seq s -> s.ToString()
        | Set s -> s.ToString()
        
    member x.Range =
        match x with
        | Empty -> <@@ false @@>
        | Seq _ -> <@ let i = var<int> in i >= 0 @> |> expand
        | Set set -> set.Range
      
    member x.Body = 
        match x with
        | Empty -> failwith "The empty set does not have a body."
        | Seq _ -> failwith "A sequence does not have a body expression. Use a set comprehension instead."  
        | Set s -> s.Body 
    
    member x.Item([<ReflectedDefinition(true)>] e: Expr<'t>) : Expr =
        match x with
        | Empty -> failwith "The empty set has no elements."
        | Seq _ -> failwith "A sequence does not have a body expression. Use a set comprehension instead."  
        | Set s -> expand (s.Body.Substitute(fun _ -> Some (e.Raw)))
    
    member x.Item(e: obj) : 't =
        match x with
        | Empty -> failwith "The empty set has no elements."
        | Seq s -> 
            match e with
            | :? int as i -> Seq.item i s
            | :? string as str ->
                match Int32.TryParse str with
                | true, i -> Seq.item i s
                | _ -> failwith "A sequence must be indexed by an integer expression."
            | :? char as c ->
                match Int32.TryParse (String([|c|])) with
                | true, i -> Seq.item i s
                | _ -> failwith "A sequence must be indexed by an integer expression."
            | _ -> failwith "A sequence must be indexed by an integer expression."
        | Set _ -> failwith "Cannot enumerate a set defined by a set comprehension. Use a sequence instead."

    /// Create a subset of a sequence using a predicate.
    member x.Subset(f:'t -> bool) = 
        match x with
        | Empty -> failwith "The empty set has no subsets."
        | Seq _ -> x |> Seq.filter f |> Set.fromSeq 
        | Set s -> failwith "Cannot use sa filter function on a set comprehension. Use a boolean expression (Expr<boo) instead."

    /// Create a subset of a sequence using a predicate.
    member x.Subset(f:Expr<bool>) = 
        match x with
        | Empty -> failwith "The empty set has no subsets."
        | Seq _ -> failwith "Cannot create a subset of a sequence using an expression. Use a filter function instead."
        | Set s -> 
            let r = s.Range';
            Set(SetComprehension(<@ %r |&| %f @>, s.Body'))

    /// Determine if the set contains an element.
    member x.HasElement elem = 
        match x, elem with
        | Empty, _ -> false
        | Seq set, e -> set.Contains e 
        | Set s, e -> s.HasElement s e
    
    /// Indicator function for an element.
    member x.Indicate elem = if x.HasElement elem then 1 else 0

    /// Determine if the set contains another set as a subset.
    member a.HasSubset b =
        match a, b with
        | Empty, _ -> false
        | _, Empty -> true
        | _, Seq _ ->  b |> Seq.forall (fun x -> a.HasElement x)
        | Seq _, Set _ ->  failwith "Cannot test if a sequence contains a set comprehension as a subset. Use 2 finite sequences or a set comprehension with a finite sequence."
        | Set _, Set _ ->  failwith "Cannot test two sets defined by set comprehensions for the subset relation. Use 2 finite sequences or a set comprehension with a finite sequence."

    member a.Difference b =
        match a, b with
        | _, Empty -> a
        | Empty, _ -> Empty
        | _,_ -> a.Subset(fun x -> b.HasElement x |> not)
        
    member a.ElementDifference b =
        match a with
        | Empty -> Empty
        | Seq _ -> Seq(a |> Seq.except [b])
        | Set s -> SetComprehension(s.Range', s.Body', (fun sc x -> s.HasElement sc x && not(x = b))) |> Set
        
    member a.Complement (b:Set<'t>) = b.Difference a
    
    /// Number of elements in the set. 
    member x.Length =
       match x with
       | Empty -> 0
       | Seq s -> x |> Seq.length
       | _ -> failwith "Cannot get the length of a set comprehension. Use a finite sequence instead."

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
                
                Seq subsets   
        | _ -> failwith "Cannot enumerate the power set of a set comprehension. Use a sequence instead."

    member x.ToSubsets() =
        match x with
        | Empty -> failwith "The empty set has no subsets."
        | Seq s -> s |> Seq.map(fun s -> Seq [s]) |> Seq
        | Set _ -> failwith "Cannot enumerate all subsets of a set comprehension. Use a sequence instead."

    member x.Product = 
        match x with
        | Empty -> Empty
        | Seq s -> Seq (cart s)
        | Set s -> 
            //let a,b = var2<'t> in
            //SetComprehension(((s.RangeTest(a)) && s.RangeTest), (s.Body', S.Body')) |> Set 
            failwith "This function is not implmented yet for a set comprehension." //let t1der.Body in SetComprehension((fun(a, b) -> builder.RangeTest a && builder.RangeTest b), <@ %t1, %t1 @>, (fun (a, b) -> x.HasElement a && x.HasElement b)) |> Set 
    
    static member fromSeq(s: seq<'t>) = Seq s
    
    static member toProduct(s:Set<'t>) = s.Product
 
    /// Set union operator.
    [<Symbol "\u222A">]
    static member (|+|) (l, r) = 
        match (l, r) with
        |(Empty, x) -> x
        |(x, Empty) -> x
        |(Seq _, Seq _) -> Seq.concat[l; r] |> Set.fromSeq //SetGenerator(Seq.concat[l; r], (fun sg x -> l.HasElement x || r.HasElement x)) |> Set.fromGen
        |_, _ -> 
            let set_union(l:Set<'t>, r: Set<'t>) = Unchecked.defaultof<'t> in
            SetComprehension(<@ set_union(l, r) @>, (fun sg x -> l.HasElement x || r.HasElement x)) |> Set
        
    /// Set intersection operator.
    [<Symbol "\u2229">]
    static member (|*|) (l, r) = 
        match (l, r) with
        |(Empty, _) -> Empty
        |(_, Empty) -> Empty
        
        |(Seq a, FiniteSeq b) -> 
                let s = 
                    Seq.concat [a;b] 
                    |> Seq.filter (fun x -> l.HasElement x && r.HasElement x) 
                    |> Seq.take (b.Count()) 
                Seq s
        |(FiniteSeq a, Seq b) -> 
                let s = 
                    Seq.concat [a;b] 
                    |> Seq.filter (fun x -> l.HasElement x && r.HasElement x) 
                    |> Seq.take (a.Count()) 
                Seq s
        |(Seq a, Seq b) -> Seq(a.Intersect b) //, (fun _ x -> l.HasElement x || r.HasElement x)) |> Set.fromGen
        |(_, _) -> 
            let set_intersect(l:Set<'t>, r: Set<'t>) = Unchecked.defaultof<'t> in
            SetComprehension(<@ set_intersect(l,r) @>, (fun sc x -> l.HasElement x && r.HasElement x)) |> Set

    ///Set 'is element of' operator
    static member (|?|)(e:'t, l:Set<'t>) = l.HasElement e

    /// Set 'is subset of' operator.
    static member (|<|) (l:Set<'t>, r:Set<'t>) = r.HasSubset l

    /// Set element difference operator.
    static member (|^|) (l:Set<'t>, r:'t) = l.ElementDifference r

    /// Set relative complement operator: A |/| B = B \ A.
    static member (|/|) (l:Set<'t>, r:Set<'t>) = l.Complement r

    /// Set absolute complement operator. -A = U \ A
    static member (~-) (l:Set<'t>) = 
        let u = SetComprehension (<@ Unchecked.defaultof<'t> @>, (fun _ _ -> true)) |> Set
        u.Difference l

    /// Set create subset operator.
    static member (|>|) (l:Set<'t>, r:'t->bool) = l.Subset r

    /// Set filter subsets operator.
    static member (|>>|) (l:Set<'t>, r:Set<'t> -> bool) = l.Powerset.Subset r

    /// Set difference operator
    static member (|-|) (l:Set<'t>, r:Set<'t>) = l.Difference r

    /// Set Cartesian product.
    static member (*) (l, r) = 
        match (l, r) with
        |(_, Empty) -> Empty
        |(Empty, _) -> Empty

        |(Seq a, Seq b) -> Seq (cart2 a b)//Seq(Gen(cart2 a b, (fun _ (x,y) -> l.HasElement x && r.HasElement y)))
        |(_,_) -> SetComprehension(<@ (l.First(), r.First()) @>, (fun sc (x,y) -> l.HasElement x && r.HasElement y)) |> Set
    
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

    /// Set element of operator.
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
    let (|^|) (l:ISet<'t>) (r:'t) = l.Set.ElementDifference r

    /// Set relative complement operator.
    let (|/|) (l:ISet<'t>) (r:ISet<'t>) = l.Set.Complement r.Set
    
    let set (range:Expr<bool>) (body:Expr<'t>) = SetComprehension(range, body) |> Set

    let subset(set: Set<'t>) (sub:Expr<bool>) = set.Subset sub

    let sequence (s: seq<'t>) = Seq s

    let infinite_seq g = g |> Seq.initInfinite |> Set.fromSeq

    let infinite_seq2 g = g |> Seq.initInfinite |> Seq.pairwise |> Set.fromSeq

    let infinite_seq3 g = g |> Seq.initInfinite |> triplewise |> Set.fromSeq

    let infinite_seq4 g = g |> Seq.initInfinite |> quadwise |> Set.fromSeq

    let infinite_seq5 g = g |> Seq.initInfinite |> quintwise |> Set.fromSeq

    let subseq(set: Set<'t>) (f:'t -> bool) = set.Subset f

    let of_type<'t when 't: equality> = fun (_:'t) -> true
    
    /// A singleton set containing 0. 
    let Zero = Singleton<Z>(0)

    /// The universal set.
    let U<'t when 't : equality> = set <@ true @> <@ Unchecked.defaultof<'t> @>  