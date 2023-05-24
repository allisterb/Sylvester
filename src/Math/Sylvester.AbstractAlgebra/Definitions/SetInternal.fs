namespace Sylvester

open System
open System.Collections

open FSharp.Quotations

/// A statement that symbolically defines a set using bound variables, range predicate, body, cardinality, and an optional F# function for computing set membership.
type SetComprehension<'t when 't: equality> internal (range:Expr<'t->bool>, mapexpr: Expr<'t->'t>, contains:SetComprehension<'t> ->'t -> bool, card:CardinalNumber) = 
    member val MapExpr = mapexpr
    member val BoundVar = param_var mapexpr
    member val Bound = Expr.Var(param_var mapexpr)
    member val Range = range
    //member val Range' = range
    member val Body = mapexpr |> body |> expand_as<'t>
    //member val Body' = body
    member val Contains = contains
    member val Cardinality = card
    
    override x.ToString() = 
        sprintf "{%s%s:%s}" (src x.Bound) (src range) (src x.Body)
     override a.GetHashCode() = (a.ToString()).GetHashCode()
     override a.Equals (_b:obj) = 
         match _b with 
         | :? SetComprehension<'t> as b -> a.ToString() = b.ToString()
         | _ -> false
    
    interface IEquatable<SetComprehension<'t>> with member a.Equals(b) = a.ToString() = b.ToString()
    
    internal new (range:Expr<'t->bool>, contains:SetComprehension<'t> ->'t -> bool, card:CardinalNumber) = SetComprehension<'t>(range,  <@ fun x -> x @>, contains, card)

    internal new (range:Expr<'t->bool>, card:CardinalNumber) = let contains = ev range in SetComprehension<'t>(range,  <@ fun x -> x @>, (fun _ x -> contains x), card)

    internal new(card:CardinalNumber) = SetComprehension<'t>(<@ fun _ -> true @>, <@ fun x -> x @>, (fun _ _ -> true), card)
  
type FiniteSequence<'t when 't: equality> internal (s:seq<'t>) = 
    do match s with
        | :? array<'t> -> ()
        | :? list<'t> ->  ()
        | o when o.GetType().IsGenericType && o.GetType().Name.StartsWith "FSharpSet" -> ()
        | _ -> failwith "This sequence is not a finite sequence."
    
    member val Seq = s
    member val Length = Seq.length s
    member x.Item(n:int) = Seq.item n s
    override a.Equals (_b:obj) = 
        match _b with 
        | :? FiniteSequence<'t> as b -> System.Linq.Enumerable.SequenceEqual(a.Seq, b.Seq)
        | :? array<'t> as b -> System.Linq.Enumerable.SequenceEqual(a.Seq, b)
        | :? list<'t> as b -> System.Linq.Enumerable.SequenceEqual(a.Seq, b)
        | _ -> false

    override a.GetHashCode() = a.Seq.GetHashCode()

    interface Generic.IEnumerable<'t> with
        member x.GetEnumerator():Generic.IEnumerator<'t> = x.Seq.GetEnumerator()
    interface IEnumerable with
        member x.GetEnumerator () = (x :> Generic.IEnumerable<'t>).GetEnumerator () :> IEnumerator

type InfiniteSequence<'t when 't: equality> internal (f:Expr<int->'t>, contains:'t->bool) = 
    let map = ev f
    member val MapExpr = f
    member val Map = map
    member val Contains = contains
    member val Seq = Seq.initInfinite map
    member val Range = <@ fun n -> n >= 0 @>
    member val Body = f |> body |> expand |> expand_as<'t>
    member x.Item(n:int) = map n
    override a.Equals (_b:obj) = 
        match _b with 
        | :? InfiniteSequence<'t> as b -> sequal a.MapExpr b.MapExpr
        | _ -> false

    override a.GetHashCode() = a.MapExpr.GetHashCode()

    override a.ToString() = sprinte a.MapExpr

    interface IEquatable<InfiniteSequence<'t>> with
        member a.Equals b = sequal a.MapExpr b.MapExpr

    interface Generic.IEnumerable<'t> with
        member x.GetEnumerator():Generic.IEnumerator<'t> = x.Seq.GetEnumerator()
    interface IEnumerable with
        member x.GetEnumerator () = (x :> Generic.IEnumerable<'t>).GetEnumerator () :> IEnumerator

type InfiniteSeries<'t when 't: equality and 't: (static member get_Zero:unit->'t) and 't: (static member (+):'t->'t->'t)> (f:Expr<int->'t>, contains:'t->bool) =
    inherit InfiniteSequence<'t>(f, contains)

[<AutoOpen>]
module internal SetInternal =      
    let (|ArraySeq|ListSeq|SetSeq|OtherSeq|) (s:seq<'t>) =
        match s with
        | :? array<'t> -> ArraySeq
        | :? list<'t> ->  ListSeq
        | o when o.GetType().IsGenericType && o.GetType().Name.StartsWith "FSharpSet" -> SetSeq
        | _ -> OtherSeq


    let cart_seq (xs:seq<'a>) (ys:seq<'b>)  = 
        match xs, ys with
        | :? FiniteSequence<'a> as a, :? FiniteSequence<'b> as b -> 
            let s = xs |> Seq.collect (fun x -> ys |> Seq.map (fun y -> x, y)) 
            FiniteSequence<'a*'b> s
        | _ -> failwith ""//InfiniteSequence<'a * 'b> (<@ fun n -> //fun(a, b)-> xc a && yc b) s 
        
    let rec cart_seql ss =
        match ss with
        | h::[] ->
            Seq.fold (fun acc elem -> [elem]::acc) [] h 
        | h::t ->
            Seq.fold (fun cacc celem ->
                (Seq.fold (fun acc elem -> (elem::celem)::acc) [] h) @ cacc) [] (cart_seql t)
        | _ -> []

    (* todo
    let rec cart_seq'' ss =
        match Seq.length ss with
        | 1 ->
            let h = Seq.item 0 ss
            Seq.fold (fun acc elem -> Seq.append [elem] acc) Seq.empty h
        | _ ->
            let h = Seq.item 0 ss
            let t = Seq.skip 0 ss
            Seq.fold (fun cacc celem -> failwith "Not implemented yet"
                //(Seq.fold (fun acc elem -> Seq.append(Seq.append([elem], celem), acc)) sSeq.empty h) @ cacc) Seq.empty (cart_seq'' t)
        | 0 -> Seq.empty
    *)
    let cart s  = cart_seq s s

    let cart2 a b = cart_seq a b

    let cart3 a b c = cart_seql [a; b; c]

    let cartn ss = cart_seql ss

    let pairwise = Seq.pairwise

    (* n-wise functions based on http://fssnip.net/50 by ptan *)

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

type SeqExpr<'t> = Expr<seq<'t>>