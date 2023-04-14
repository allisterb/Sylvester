namespace Sylvester

open System
open System.Collections

open FSharp.Quotations

/// A statement that symbolically defines a set using bound variables, range predicate, body, cardinality, and an optional F# function for computing set membership.
type SetComprehension<'t when 't: equality> internal (bound:Expr<'t>, range:Expr<bool>, body: Expr<'t>, card:CardinalNumber, ?hasElement:SetComprehension<'t> ->'t -> bool) = 
    member val Bound = 
        match bound with
        | Patterns.BoundVars v -> bound
        | _ -> failwithf "The expression %s is not recognized as a bound variable(s) expression." (src bound)
    member val Range = expand range
    member val Range' = range
    member val Body = expand body
    member val Body' = body
    member val HasElement = defaultArg hasElement (fun (sc:SetComprehension<'t>) (_:'t) -> failwithf "No set membership function is defined for the set comprehension %A." sc)
    member val Cardinality = card
    
    override x.ToString() = 
        let vars = body |> get_vars
        let v = if Seq.isEmpty vars then "" else vars.[0].ToString() + "|"
        sprintf "{%s%s:%s}" v (src range) (src body)
     override a.GetHashCode() = (a.ToString()).GetHashCode()
     override a.Equals (_b:obj) = 
         match _b with 
         | :? SetComprehension<'t> as b -> (a :> IEquatable<SetComprehension<'t>>).Equals b
         | _ -> false
    interface IEquatable<SetComprehension<'t>> with member a.Equals(b) = a.ToString() = b.ToString()
    
    internal new(bound:Expr<'t>, body:Expr<'t>, card:CardinalNumber) = SetComprehension(bound, <@ true @>, body, card, fun _ _ -> true)
        
    internal new (range:Expr<bool>, body:Expr<'t>, card:CardinalNumber, ?hasElement: SetComprehension<'t> -> 't -> bool) =
        let b = get_vars_to_tuple range
        match hasElement with
        | Some h -> SetComprehension(<@ %%b:'t @>, range, body, card, h)
        | None -> SetComprehension(<@ %%b:'t @>, range, body, card)

    internal new (body:Expr<'t>, card:CardinalNumber) =
        let b = get_vars_to_tuple body
        SetComprehension(<@ %%b:'t @>, <@ true @>, body, card, fun _ _ -> true)
    
    internal new (pred:Expr<'t->bool>, card:CardinalNumber) =
        let p = evaluate pred
        let b' = body pred
        let v = pred |> param_vars |> vars_to_tuple
        SetComprehension(<@ %%v:'t @>, <@ %%b':bool @>, <@ %%v:'t @>, card, fun _ x -> p x)

type internal SequenceGenerator<'t when 't: equality> (s:seq<'t>, isInfinite:bool, ?contains:'t->bool) = 
    member val Sequence = s
    member val IsInfinite = isInfinite
    member val Contains = 
        match isInfinite with
        | false -> fun (e:'t) -> Seq.contains e s
        | true -> if contains.IsNone then failwith "" else fun (e:'t) -> contains.Value e

    interface Generic.IEnumerable<'t> with
        member x.GetEnumerator():Generic.IEnumerator<'t> = x.Sequence.GetEnumerator()
    interface IEnumerable with
        member x.GetEnumerator () = (x :> Generic.IEnumerable<'t>).GetEnumerator () :> IEnumerator
          
[<AutoOpen>]
module internal SetInternal =
    let (|ArraySymSeq|ListSymSeq|SetSymSeq|InfiniteGenSymSeq|FiniteGenSymSeq|OtherSymSeq|) (s:Generic.IEnumerable<Term<'t>>) =
        match s with
            | :? array<Term<'t>> -> ArraySymSeq
            | :? list<Term<'t>> ->  ListSymSeq
            | o when o.GetType().IsGenericType && o.GetType().Name.StartsWith "FSharpSet" && o.GetType().GenericTypeArguments.[0].Name.StartsWith("Sylvester.Sym") -> SetSymSeq
            | :? SequenceGenerator<Term<'t>> as g when not g.IsInfinite -> FiniteGenSymSeq
            | :? SequenceGenerator<Term<'t>> as g when g.IsInfinite -> InfiniteGenSymSeq
            | _ -> OtherSymSeq
         
    let (|ArraySeq|ListSeq|SetSeq|InfiniteGenSeq|FiniteGenSeq|OtherSeq|) (s:seq<'t>) =
        match s with
        | :? array<'t> -> ArraySeq
        | :? list<'t> ->  ListSeq
        | o when o.GetType().IsGenericType && o.GetType().Name.StartsWith "FSharpSet" -> SetSeq
        | :? SequenceGenerator<'t> as g when not g.IsInfinite -> FiniteGenSeq
        | :? SequenceGenerator<'t> as g when g.IsInfinite -> InfiniteGenSeq
        
        | :? seq<Term<'t>> as se -> 
            match se with
            | ArraySymSeq -> ArraySeq
            | ListSymSeq -> ListSeq
            | SetSymSeq -> SetSeq
            | InfiniteGenSymSeq -> InfiniteGenSeq
            | FiniteGenSymSeq -> FiniteGenSeq
            | _ -> OtherSeq
        | _ -> OtherSeq

    let (|FiniteSeq|_|) (x:seq<'t>) =
        match x with
        | ArraySeq
        | ListSeq
        | SetSeq 
        | FiniteGenSeq -> Some (SequenceGenerator<'t>(x, false))
        | _ -> None

    let (|InfiniteSeq|_|) (x:seq<'t>) =
        match x with
        | InfiniteGenSeq -> Some (x :?> SequenceGenerator<'t>)
        | _ -> None
    
    let finite_seq_gen<'t when 't: equality> s = SequenceGenerator<'t>(s, false) :> seq<'t>

    let infinite_seq_gen<'t when 't: equality> (c:'t->bool) s  = SequenceGenerator<'t>(s, true, c) :> seq<'t>
    
    let cart_seq (xc:'a->bool) (yc:'b->bool) (xs:seq<'a>) (ys:seq<'b>)  = 
        let s = xs |> Seq.collect (fun x -> ys |> Seq.map (fun y -> x, y)) 
        match xs, ys with
        | FiniteSeq _, FiniteSeq _ -> finite_seq_gen s
        | _ -> infinite_seq_gen<'a * 'b> (fun(a, b)-> xc a && yc b) s 
        
    let rec cart_seq' ss =
        match ss with
        | h::[] ->
            Seq.fold (fun acc elem -> [elem]::acc) [] h 
        | h::t ->
            Seq.fold (fun cacc celem ->
                (Seq.fold (fun acc elem -> (elem::celem)::acc) [] h) @ cacc) [] (cart_seq' t)
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
    let cart s c = cart_seq c c s s

    let cart2 a b = cart_seq a b

    let cart3 a b c = cart_seq' [a; b; c]

    let cartn ss = cart_seq' ss

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