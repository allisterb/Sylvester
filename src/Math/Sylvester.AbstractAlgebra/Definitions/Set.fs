namespace Sylvester

open System
open System.Collections
open System.Collections.Generic
open System.Linq
open System.Reflection
open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns

open Arithmetic
open Sylvester.Collections

/// A set of elements each with type or class denoted by t.
[<CustomEquality; CustomComparison>]
type Set<'t when 't: equality> =
/// The empty set.
| Empty
/// A set defined by the distinct unordered values of a sequence.
| Seq of seq<'t>
/// A set symbolically defined by a bound variable, range and body expression. 
| Set of SetComprehension<'t>
with          
    interface IEnumerable<'t> with
        member x.GetEnumerator () = 
            match x with
            |Empty -> Seq.empty.GetEnumerator()
            |Seq s -> 
                match s with
                | FiniteSeq _ 
                | InfiniteSeq _ -> let ds = Seq.distinct s in ds.GetEnumerator()
                | _ -> failwithf "Cannot determine the cardinality of this sequence expression %A. Use a list, array, or sequence generator." s
            | Set _ -> failwith "Cannot enumerate the members of a set comprehension. Use a sequence instead."
        member x.GetEnumerator () = (x :> IEnumerable<'t>).GetEnumerator () :> IEnumerator

    interface IEquatable<Set<'t>> with
        member a.Equals b =
            match a, b with
            | Empty, Empty -> true
            | _, Empty -> false
            | Empty, _ -> false
            | Set expr1, Set expr2 ->  expr1 = expr2
            | Seq (FiniteSeq _), Seq (InfiniteSeq _)
            | Seq (InfiniteSeq _), Seq (FiniteSeq _)-> false
            | Seq (FiniteSeq s1), Seq (FiniteSeq s2) ->  System.Linq.Enumerable.SequenceEqual(s1, s2)            
            | Seq (InfiniteSeq s1), Seq (InfiniteSeq s2) -> s1.Equals s2 
            |_,_ -> failwith "Cannot test a sequence and a set comprehension for equality. Use 2 sequences or 2 set comprehensions."
    
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
        | Seq s -> s.GetHashCode()
        | Set p -> p.GetHashCode()

    override x.ToString() =
        match x with
        | Empty -> "Empty"
        | Seq s -> s.ToString()
        | Set s -> s.ToString()
        
    member x.Cardinality =      
        match x with
        | Empty -> lazy 0 |> Finite 
        | Seq s ->
            match s with
            | FiniteSeq _ -> lazy(x |> Seq.length) |> Finite 
            | InfiniteSeq _ -> Aleph 0 
            | _ -> failwithf "Cannot determine the cardinality of this sequence expression %s. Use a list, array, or sequence generator." (s.GetType().Name)
        | Set sc -> sc.Cardinality
        
    member x.Range =
        match x with
        | Empty -> <@@ false @@>
        | Seq _ -> let i = var'<int> "i" in <@ %i >= 0 @> |> expand
        | Set set -> set.Range
      
    member x.Body = 
        match x with
        | Empty -> failwith "The empty set does not have a body."
        | Seq _ -> failwith "A sequence does not have a body expression. Use a set comprehension instead."  
        | Set s -> s.Body 
    
    (*
    member x.Item([<ReflectedDefinition(true)>] e: Expr<'t>) : Expr<'t> =
        match x with
        | Empty -> failwith "The empty set has no elements."
        | Seq se -> 
            if typeof<'t> = typeof<int> then 
                let i = box((getExprFromReflectedDefinition<'t> e)) :?> int
                let v = Seq.item i se in Expr.Value v |> expand_as<'t>
            else
                failwith "A sequence does not have a body expression. Use a set comprehension instead."  
        | Set sc -> expand (sc.Body.Substitute(fun _ -> Some (e.Raw))) |> expand_as<'t>
    *)
    member x.Item(e: obj) : 't =
        match x with
        | Empty -> failwith "The empty set has no elements."
        | Seq s -> 
            match e with
            | :? int as i -> Seq.item i s
            | :? uint32 as i -> Seq.item ((int) i) s
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

    /// Determine if the set contains an element.
    member x.HasElement elem = 
        match x, elem with
        | Empty, _ -> false
        | Seq _, e -> x.Contains e 
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

    /// Create a subset of a set using a filter predicate.
    member x.Subset(f':Expr<'t -> bool>) = 
        let f = evaluate f'
        match x with
        | Empty -> failwith "The empty set has no subsets."
        | Seq s -> 
            match s with
            | FiniteSeq _ -> x |> Seq.filter f |> finite_seq_gen |> Set.fromSeq
            | InfiniteSeq _ -> x |> Seq.filter f |> infinite_seq_gen |> Set.fromSeq
            | _ -> failwithf "Cannot determine the cardinality of the sequence expression %s. Use a list, array, or sequence generator." (s.GetType().Name)
        | Set s -> 
            let r = s.Range'
            let nr = body f'
            Set(SetComprehension(s.Bound, <@ %r |&| (%%nr:bool) @>, s.Body', s.Cardinality, fun _ e -> x.HasElement e && f e))

    member a.Difference b =
        match a, b with
        | _, Empty -> a
        | Empty, _ -> Empty
        | Seq (FiniteSeq s1), Seq s2 -> s1 |> Seq.except s2 |> finite_seq_gen |>Set.fromSeq
        | Seq (InfiniteSeq s1), Seq s2 -> s1 |> Seq.except s2 |> infinite_seq_gen |>Set.fromSeq
        | _,_ -> a.Subset(<@ fun x -> b.HasElement x |> not @>)
        
    member a.ElementDifference b =
        match a with
        | Empty -> Empty
        | Seq (FiniteSeq s) -> s |> Seq.except [b] |> finite_seq_gen |> Set.fromSeq
        | Seq s -> s |> Seq.except [b] |> infinite_seq_gen |> Set.fromSeq
        | Set s -> SetComprehension(s.Bound, s.Range', s.Body', (a.Cardinality - (Finite (lazy 1))), (fun sc x -> s.HasElement sc x && not(x = b))) |> Set
        
    member a.Complement (b:Set<'t>) = b.Difference a
           
    /// Set of all subsets.
    member a.Powerset : Set<Set<'t>>=
        match a with
        | Empty -> Empty
        | Seq _s ->
            let subsets = 
                let singleton e = Seq.singleton e
                let append e se = Seq.append (singleton e) se
                Seq.foldBack (fun x rest -> Seq.append rest (Seq.map (fun ys -> (append x ys)) rest)) _s (singleton Seq.empty)
                |> Seq.map(fun s -> if Seq.isEmpty s then Empty else Seq(s))
            Seq subsets

        | _ -> failwith "Cannot enumerate the power set of a set comprehension. Use a sequence instead."

    member x.EnumAsSubsets() =
        match x with
        | Empty -> failwith "The empty set has no elements."
        | Seq s -> s |> Seq.map(fun s -> Seq [s]) |> Seq
        | Set _ -> failwith "Cannot enumerate elements of a set comprehension. Use a sequence instead."
    
    static member fromSeq(s: seq<'t>) = Seq s
    
    /// Set union opera
    [<Symbol "\u222A">]
    static member (|+|) (l, r) = 
        match (l, r) with
        |(Empty, x) -> x
        |(x, Empty) -> x
        |(Seq _, Seq _) -> Seq.concat[l; r] |> Set.fromSeq //SetGenerator(Seq.concat[l; r], (fun sg x -> l.HasElement x || r.HasElement x)) |> Set.fromGen
        |Set a, _ -> 
            let set_union(l:Set<'t>, r: Set<'t>) = formula<'t> in
            SetComprehension(a.Bound, a.Range', <@ set_union(l, r) @>, (l.Cardinality + r.Cardinality), (fun sg x -> l.HasElement x || r.HasElement x)) |> Set
        |_, Set b -> 
            let set_union(l:Set<'t>, r: Set<'t>) = formula<'t> in
            SetComprehension(b.Bound, b.Range', <@ set_union(l, r) @>, (l.Cardinality + r.Cardinality), (fun sg x -> l.HasElement x || r.HasElement x)) |> Set
    
    [<Symbol "\u222A">]
    static member (|+|) (l:ISet<'t>, r:Set<'t>) = l.Set |+| r 

    [<Symbol "\u222A">]
    static member (|+|) (l:Set<'t>, r:ISet<'t>) = l |+| r.Set
    
    /// Set intersection operator.
    [<Symbol "\u2229">]
    static member (|*|) (l, r) = 
        match (l, r) with
        |(Empty, _) -> Empty
        |(_, Empty) -> Empty        
        |(Seq a, Seq b) -> Seq(a.Intersect b) 
        |(Set a, _) -> 
            let set_intersect(l:Set<'t>, r: Set<'t>) = formula<'t> in
            SetComprehension(a.Bound, a.Range', <@ set_intersect(l,r) @>, (l.Cardinality / r.Cardinality), (fun sc x -> l.HasElement x && r.HasElement x)) |> Set
        |(_, Set b) -> 
            let set_intersect(l:Set<'t>, r: Set<'t>) = formula<'t> in
            SetComprehension(b.Bound, b.Range', <@ set_intersect(l,r) @>, (l.Cardinality / r.Cardinality), (fun sc x -> l.HasElement x && r.HasElement x)) |> Set

    ///Set 'is element of' operator
    static member (|?|)(e:'t, l:Set<'t>) = l.HasElement e

    static member (|?|) (l:Term<'t>, r:Set<'t>) : ScalarTerm<bool> = 
        let m = typeof<Set<'t>>.GetMethod("op_BarQmarkBar", (FSharp.Core.Operators.(|||) BindingFlags.Public BindingFlags.Static), System.Type.DefaultBinder, [| typeof<'t>; typeof<Set<'t>> |], [||]) in
        binary_call(None, m, l.Expr, Expr.Value r) |> expand_as<bool> |> ScalarTerm<bool>
    
    /// Set 'is subset of' operator.
    static member (|<|) (l:Set<'t>, r:Set<'t>) = r.HasSubset l

    /// Set 'is subset of' operator.
    static member (|<|) (l:Set<'t>, r:ISet<'t>) = r.Set.HasSubset l

    /// Set 'is subset of' operator.
    static member (|<|) (l:ISet<'t>, r:Set<'t>) = r.HasSubset l.Set

    /// Set element difference operator.
    static member (|^|) (l:Set<'t>, r:'t) = l.ElementDifference r

    /// Set element difference operator.
    static member (|^|) (l:ISet<'t>, r:'t) = l.Set.ElementDifference r

    /// Set relative complement operator: A |/| B = B \ A.
    static member (|/|) (l:Set<'t>, r:Set<'t>) = l.Complement r

    /// Set absolute complement operator. -A = U \ A
    static member (~-) (l:Set<'t>) = l.Complement Set.U

    /// Set create subset operator.
    static member (|>|) (l:Set<'t>, [<ReflectedDefinition>] r:Expr<'t->bool>) = l.Subset r

    /// Set filter subsets operator.
    static member (|>>|) (l:Set<'t>, r:Expr<Set<'t> -> bool>) = l.Powerset.Subset r

    /// Set difference operator
    static member (|-|) (l:Set<'t>, r:Set<'t>) = l.Difference r
    
/// Set Cartesian product.
    static member (*) (l:Set<'a>, r:Set<'b>) = 
        match (l, r) with
        |(_, Empty) -> Empty
        |(Empty, _) -> Empty
        |(Seq x, Seq y) -> 
            let a = cart_seq x y
            let c =
                match l.Cardinality, r.Cardinality with
                | Finite _, _
                | _, Finite _-> (finite_seq_gen a) |> Set.fromSeq
                | Aleph _, Aleph _ -> infinite_seq_gen(Seq.initInfinite (fun n -> Seq.item n a) ) |> Set.fromSeq
            c
        |(_,_) -> let a, b = var'<'a> "a", var'<'b> "b" in SetComprehension<'a * 'b>(<@  (%a, %b) @>, <@ true @>, <@  (%a, %b) @>, (l.Cardinality * r.Cardinality), (fun sc (x,y) -> (l.HasElement x) |&| (r.HasElement y))) |> Set

    interface ISet<'t> with member x.Set = x

    /// The universal set.
    static member U = 
        let x = var'<'t> "x" in Set(SetComprehension(<@ %x @>, <@ true @>, <@ %x @>, default_card<'t>, fun _ _ -> true)) 
    
    /// A singleton set containing 0. 
    static member Zero = Singleton<int>(0)

    
and SetFamily<'t when 't : equality> = Set<Set<'t>> 

and KnownSet<'n, 't when 'n :> Number and 't : equality>([<ParamArray>] items: 't[]) =
    member val Size = number<'n>
    member val Items = Array<'n, 't>(items)
    member val Set = Seq items
    interface IKnownSet<'n, 't> with
        member x.Set = x.Set
        member x.Equals y = x.Set.Equals y
        member x.Size = x.Size
    interface Generic.IEnumerable<'t> with
        member x.GetEnumerator():Generic.IEnumerator<'t> = (x.Set :> IEnumerable<'t>).GetEnumerator()
        member x.GetEnumerator():IEnumerator = (x.Set :> IEnumerable).GetEnumerator()
    new (items:seq<'t>) = KnownSet(items |> Seq.toArray)

and Singleton<'t when 't: equality>(e:'t) = inherit KnownSet<dim<1>, 't>([|e|])

and ISet<'t when 't: equality> = 
    inherit IEquatable<Set<'t>>
    abstract member Set:Set<'t>

and IKnownSet<'n, 't when 'n :> Number and 't : equality> = 
    inherit ISet<'t>
    abstract Size: 'n

[<RequireQualifiedAccess>]
module SetOps =
    let union<'t when 't: equality> = typeof<Set<'t>>.GetMethod("op_BarPlusBar", (FSharp.Core.Operators.(|||) BindingFlags.Public BindingFlags.Static), System.Type.DefaultBinder, 
                                        [| typeof<Set<'t>>; typeof<Set<'t>> |], [||])

    let intersection<'t when 't: equality> = typeof<Set<'t>>.GetMethod("op_BarMultiplyBar", (FSharp.Core.Operators.(|||) BindingFlags.Public BindingFlags.Static), System.Type.DefaultBinder, 
                                                [| typeof<Set<'t>>; typeof<Set<'t>> |], [||])

    let elementOf<'t when 't: equality> = typeof<Set<'t>>.GetMethod("op_BarQmarkBar", (FSharp.Core.Operators.(|||) BindingFlags.Public BindingFlags.Static), System.Type.DefaultBinder, 
                                            [| typeof<'t>; typeof<Set<'t>> |], [||])

    let subsetOf<'t when 't: equality> = typeof<Set<'t>>.GetMethod("op_BarLessBar", (FSharp.Core.Operators.(|||) BindingFlags.Public BindingFlags.Static), System.Type.DefaultBinder, 
                                            [| typeof<Set<'t>>; typeof<Set<'t>> |], [||])

    let elementDifference<'t when 't: equality> = typeof<Set<'t>>.GetMethod("op_BarHatBar", (FSharp.Core.Operators.(|||) BindingFlags.Public BindingFlags.Static), System.Type.DefaultBinder, 
                                                    [| typeof<Set<'t>>; typeof<'t> |], [||])

    let complement<'t when 't: equality> = typeof<Set<'t>>.GetMethod("op_BarDivideBar", (FSharp.Core.Operators.(|||) BindingFlags.Public BindingFlags.Static), System.Type.DefaultBinder, 
                                            [| typeof<Set<'t>>; typeof<Set<'t>> |], [||])

    let absoluteComplement<'t when 't: equality> = typeof<Set<'t>>.GetMethod("op_UnaryNegation", (FSharp.Core.Operators.(|||) BindingFlags.Public BindingFlags.Static), System.Type.DefaultBinder, 
                                                    [| typeof<Set<'t>> |], [||])

    let createSubset<'t when 't: equality> = typeof<Set<'t>>.GetMethod("op_BarGreaterBar", (FSharp.Core.Operators.(|||) BindingFlags.Public BindingFlags.Static), System.Type.DefaultBinder, 
                                                [| typeof<Set<'t>>; typeof<Expr<'t -> bool>> |], [||])

    let filterSubsets<'t when 't: equality> = typeof<Set<'t>>.GetMethod("op_BarGreaterGreaterBar", (FSharp.Core.Operators.(|||) BindingFlags.Public BindingFlags.Static), System.Type.DefaultBinder, 
                                                [| typeof<Set<'t>>; typeof<Expr<Set<'t> -> bool>> |], [||])

    let rec sprintset<'t when 't: equality> (x:Expr<Set<'t>>) = 
        match x with
        | List list -> "[" + (list |>  List.map (sprintset<'t> << expand_as<Set<'t>>) |> List.reduce (fun l r -> l + ", " + r)) + "]"
        //| NewUnionCase (s, b) when s.
        | Call (_, m, [l; r]) when m.Name = union<'t>.Name -> sprintf("%s |+| %s") ((sprintset << expand_as<Set<'t>>) l) ((sprintset << expand_as<Set<'t>>) r)
        | _ -> src x 

type SetTerm<'t when 't: equality>(expr:Expr<Set<'t>>) =
    inherit Term<Set<'t>>(expr) 
    
    override a.Display = SetOps.sprintset a.Expr

    static member (|+|) (l:SetTerm<'t>, r:Set<'t>) = binary_call(None, SetOps.union<'t>, l.Expr, Expr.Value r) |> expand_as<Set<'t>> |> SetTerm

    static member (|+|) (l:Set<'t>, r:SetTerm<'t>) = binary_call(None, SetOps.union<'t>, Expr.Value l, r.Expr) |> expand_as<Set<'t>> |> SetTerm

    static member (|+|) (l:SetTerm<'t>, r:SetTerm<'t>) = binary_call(None, SetOps.union<'t>, l.Expr, r.Expr) |> expand_as<Set<'t>> |> SetTerm

    static member (|+|) (l:SetTerm<'t>, r:ISet<'t>) = binary_call(None, SetOps.union<'t>, l.Expr, Expr.Value r.Set) |> expand_as<Set<'t>> |> SetTerm

    static member (|+|) (l:ISet<'t>, r:SetTerm<'t>) = binary_call(None, SetOps.union<'t>, Expr.Value l.Set, r.Expr) |> expand_as<Set<'t>> |> SetTerm

    static member (|*|) (l:SetTerm<'t>, r:Set<'t>) = binary_call(None, SetOps.intersection<'t>, l.Expr, Expr.Value r) |> expand_as<Set<'t>> |> SetTerm

    static member (|*|) (l:Set<'t>, r:SetTerm<'t>) = binary_call(None, SetOps.intersection<'t>, Expr.Value(l), r.Expr) |> expand_as<Set<'t>> |> SetTerm

    static member (|*|) (l:SetTerm<'t>, r:SetTerm<'t>) = binary_call(None, SetOps.intersection<'t>, l.Expr, r.Expr) |> expand_as<Set<'t>> |> SetTerm

    static member (|*|) (l:SetTerm<'t>, r:ISet<'t>) = binary_call(None, SetOps.intersection<'t>, l.Expr, Expr.Value r.Set) |> expand_as<Set<'t>> |> SetTerm

    static member (|*|) (l:ISet<'t>, r:SetTerm<'t>) = binary_call(None, SetOps.intersection<'t>, Expr.Value l.Set, r.Expr) |> expand_as<Set<'t>> |> SetTerm

    static member (|?|) (l:Term<'t>, r:SetTerm<'t>) : ScalarTerm<bool> = binary_call(None, SetOps.elementOf<'t>, l.Expr, r.Expr) |> expand_as<bool> |> ScalarTerm<bool>

    static member (|?|) (l:'t, r:SetTerm<'t>) : ScalarTerm<bool> = binary_call(None, SetOps.elementOf<'t>, Expr.Value l, r.Expr) |> expand_as<bool> |> ScalarTerm<bool>
    
    static member (|<|) (l:SetTerm<'t>, r:Set<'t>) = binary_call(None, SetOps.subsetOf<'t>, l.Expr, Expr.Value r) |> expand_as<Set<'t>> |> SetTerm

    static member (|<|) (l:Set<'t>, r:SetTerm<'t>) = binary_call(None, SetOps.subsetOf<'t>, Expr.Value(l), r.Expr) |> expand_as<Set<'t>> |> SetTerm

    static member (|<|) (l:SetTerm<'t>, r:SetTerm<'t>) = binary_call(None, SetOps.subsetOf<'t>, l.Expr, r.Expr) |> expand_as<Set<'t>> |> SetTerm

    static member (|<|) (l:SetTerm<'t>, r:ISet<'t>) = binary_call(None, SetOps.subsetOf<'t>, l.Expr, Expr.Value r.Set) |> expand_as<Set<'t>> |> SetTerm

    static member (|<|) (l:ISet<'t>, r:SetTerm<'t>) = binary_call(None, SetOps.subsetOf<'t>, Expr.Value l.Set, r.Expr) |> expand_as<Set<'t>> |> SetTerm

    static member (|^|) (l:SetTerm<'t>, r:'t) : SetTerm<'t> = binary_call(None, SetOps.elementDifference<'t>, l.Expr, Expr.Value r) |> expand_as<Set<'t>> |> SetTerm

    static member (|^|) (l:SetTerm<'t>, r:Term<'t>) : SetTerm<'t> = binary_call(None, SetOps.elementDifference<'t>, l.Expr, r.Expr) |> expand_as<Set<'t>> |> SetTerm

    static member (|^|) (l:Set<'t>, r:Term<'t>) : SetTerm<'t> = binary_call(None, SetOps.complement<'t>, Expr.Value l, r.Expr) |> expand_as<Set<'t>> |> SetTerm

    static member (|/|) (l:SetTerm<'t>, r:Set<'t>) = binary_call(None, SetOps.complement<'t>, l.Expr, Expr.Value r) |> expand_as<Set<'t>> |> SetTerm

    static member (|/|) (l:Set<'t>, r:SetTerm<'t>) = binary_call(None, SetOps.complement<'t>, Expr.Value l, r.Expr) |> expand_as<Set<'t>> |> SetTerm

    static member (|/|) (l:SetTerm<'t>, r:SetTerm<'t>) = binary_call(None, SetOps.complement<'t>, l.Expr, r.Expr) |> expand_as<Set<'t>> |> SetTerm

    static member (|/|) (l:SetTerm<'t>, r:ISet<'t>) = binary_call(None, SetOps.complement<'t>, l.Expr, Expr.Value r.Set) |> expand_as<Set<'t>> |> SetTerm

    static member (|/|) (l:ISet<'t>, r:SetTerm<'t>) = binary_call(None, SetOps.complement<'t>, Expr.Value l.Set, r.Expr) |> expand_as<Set<'t>> |> SetTerm

    static member (~-) (l:SetTerm<'t>) = unary_call(None, SetOps.absoluteComplement<'t>, l.Expr) |> expand_as<Set<'t>> |> SetTerm

    static member (|>|) (l:SetTerm<'t>, r:Expr<'t->bool>) = binary_call(None, SetOps.createSubset<'t>, l.Expr, r) |> expand_as<Set<'t>> |> SetTerm
    
    static member (|>>|) (l:SetTerm<'t>, r:Expr<Set<'t>->bool>) = binary_call(None, SetOps.filterSubsets<'t>, l.Expr, r) |> expand_as<Set<Set<'t>>> |> SetTerm

and SetVar<'t when 't: equality>(n: string) = 
    inherit SetTerm<'t>(Expr.Var(Var(n, typeof<Set<'t>>)) |> expand_as<Set<'t>>)
    member x.Name = n
    member x.Var = match x.Expr with | Var v -> v | _ -> failwith ""
    member x.Item(i:IndexVar) = IndexedSetVar(x, i)
    member x.Item(i:int) = SetVar(x.Name + i.ToString())

and IndexedSetVar<'t when 't: equality>(var:SetVar<'t>, index:IndexVar) =
    inherit SetVar<'t>(var.Name + "_" + index.Name)
    member x.Item(i:int) = SetVar(x.Name.Replace("_" + index.Name, i.ToString()))

[<AutoOpen>]
module Set =
    let set_of<'t when 't : equality> (s:ISet<'t>) = s.Set

    let set_union (l:Set<'t>) (r:Set<'t>) = l |+| r

    let set_intersection (l:Set<'t>) (r:Set<'t>) = l |*| r

    let not_empty (l:ISet<'t>) = l.Set <> Empty

    let card (s:ISet<'t>) = s.Set.Cardinality
  
    let measure (s:ISet<'t>) = let c = (card s) in c.Measure()

    let set<'t when 't: equality> (range:Expr<bool>) (body:Expr<'t>) card = SetComprehension(range, body, card) |> Set :> ISet<'t>

    let finite_set (range:Expr<bool>) (body:Expr<'t>) n = SetComprehension(range, body, (lazy n) |> Finite) |> Set 
    
    let infinite_set (range:Expr<bool>) (body:Expr<'t>) n = SetComprehension(range, body, Aleph n) |> Set  

    let countable_set range (body:Expr<'t>) = infinite_set range body 0

    let uncountable_set range (body:Expr<'t>) = infinite_set range body 1

    let set_from_pred<'t when 't: equality>(p:Expr<'t -> bool>) = SetComprehension<'t>(p, default_card<'t>) |> Set

    let singleton<'t when 't: equality> (e:'t) = Singleton e

    let subset (sub:Expr<'t->bool>) (set: ISet<'t>) = set.Set.Subset sub

    let sseq(s:seq<'t>) = (Seq s)

    let sseq2 (s: seq<'t>) = s |> cart |> Set.fromSeq

    let sseqp2 (s: seq<'t>) = s |> pairwise |> Set.fromSeq
    
    let enum_as_subsets (set:Set<'t>) = set.EnumAsSubsets()

    let finite_seq s = s |> finite_seq_gen |> Set.fromSeq

    let infinite_seq<'t when 't:equality> g = Seq.initInfinite<'t> g  |> Seq.skip 1 |> infinite_seq_gen<'t>
    
    let infinite_seq'<'t when 't:equality> (f: Expr<int ->'t ->'t>) =
        let vf = get_vars f |> List.head
        infinite_seq_gen(Seq.initInfinite (fun i -> 
                    let b = (body f).Substitute(fun v -> if v.Name = vf.Name && v.Type = vf.Type then Some(Expr.Value i) else None)
                    <@ (%%b:'t) @>)) 
        |> Seq.map ScalarTerm<'t>
        |> Seq.skip 1
        |> infinite_seq_gen<ScalarTerm<'t>> 
       
    let infinite_seq2 g = g |> cart
        
    let infinite_seqp2 g = g |> infinite_seq_gen |> pairwise

    let infinite_seqp3 g = g |> infinite_seq_gen |> triplewise  

    let infinite_seqp4 g = g |> infinite_seq_gen |> quadwise 

    let infinite_seqp5 g = g |> infinite_seq_gen |> quintwise 

    let inline series s = Seq.scan (+) LanguagePrimitives.GenericZero s |> Seq.skip 1 

    let inline partial_sum (n:int) s = s |> (series >> Seq.item n)
    
    let inline infinite_series g = g |> (infinite_seq >> series)
    
    //let inline series' s = Seq.scan (Term<'t>.add) (Term.zero()) s |> Seq.skip 1

    //let inline partial_sum' (n:int) s = s |> (series' >> Seq.item n)
    
    //let inline infinite_series' g = g |> (infinite_seq' >> series')

    //let setvar<'t when 't : equality> n = var'<Set<'t>> n
    
    let setvar<'t when 't: equality> name = SetVar<'t> name
    
    let setvar2<'t when 't : equality> n o = setvar n, setvar o

    let setvar3<'t when 't : equality> n o p = setvar n, setvar o, setvar p
    
    let setvar4<'t when 't : equality> n o p q = setvar n, setvar o, setvar p, setvar q

    let fail_if_set_not_eq (a:ISet<'t>) (b:ISet<'t>) = if not (a.Set = b.Set) then failwithf "The set %A is not equal to the set %A." a b

    type uninterp = obj