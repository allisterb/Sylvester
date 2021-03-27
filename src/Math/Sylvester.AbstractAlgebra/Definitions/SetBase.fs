namespace Sylvester

open System
open System.Collections

open FSharp.Quotations

[<CustomEquality; NoComparison>]
type Sym<'t> = Sym of Expr<'t> with
     member x.Expr = let (Sym e) = x in e
     interface IEquatable<Sym<'t>> with member a.Equals b = a.Expr.ToString() = b.Expr.ToString()
     override a.GetHashCode() = (a.Expr.ToString()).GetHashCode()
     override a.Equals (_b:obj) = 
             match _b with 
             | :? Sym<'t> as e -> (a :> IEquatable<Sym<'t>>).Equals e
             | _ -> false
     override x.ToString() = src (x.Expr)

type any = Sym<obj>

/// A statement that formally defines a set using a range, body, cardinality, and an optional F# function for computing set membership.
type SetComprehension<'t when 't: equality>(range:Expr<bool>, body: Expr<'t>, card:CardinalNumber, ?hasElement:SetComprehension<'t> ->'t -> bool) = 
    member val Range = expand range
    member val internal Range' = range
    member val Body = expand body
    member val internal Body' = body
    member val HasElement = defaultArg hasElement (fun (sc:SetComprehension<'t>) (_:'t) -> failwithf "No set membership function is defined for the set comprehension %A." sc)
    member val Cardinality = card
    interface IEquatable<SetComprehension<'t>> with member a.Equals(b) = a.ToString() = b.ToString()
    override a.GetHashCode() = (a.ToString()).GetHashCode()
    override a.Equals (_b:obj) = 
            match _b with 
            | :? SetComprehension<'t> as b -> (a :> IEquatable<SetComprehension<'t>>).Equals b
            | _ -> false
    override x.ToString() = 
        let vars = body |> get_vars
        let v = if Seq.isEmpty vars then "" else vars.[0].ToString() + "|"
        sprintf "{%s%s:%s}" v (src range) (src body)

    new(body: Expr<'t>, card:CardinalNumber, hasElement: SetComprehension<'t> -> 't -> bool) = 
        let b = getExprFromReflectedDefinition<'t> body in 
        SetComprehension(<@ true @>, body, card, hasElement)

type InfiniteSequenceGen<'t when 't: equality> (fn: int -> 't) = 
    member x.Function = fn
    member x.Sequence = Seq.initInfinite x.Function
    interface Generic.IEnumerable<'t> with
        member x.GetEnumerator():Generic.IEnumerator<'t> = let s = Seq.distinct x.Sequence in s.GetEnumerator()
    interface IEnumerable with
        member x.GetEnumerator () = (x :> Generic.IEnumerable<'t>).GetEnumerator () :> IEnumerator

type FiniteSequenceGen<'t when 't: equality> (s:seq<'t>) = 
    member x.Sequence = s
    interface Generic.IEnumerable<'t> with
        member x.GetEnumerator():Generic.IEnumerator<'t> = let s = Seq.distinct x.Sequence in s.GetEnumerator()
    interface IEnumerable with
        member x.GetEnumerator () = (x :> Generic.IEnumerable<'t>).GetEnumerator () :> IEnumerator
        
[<AutoOpen>]
module SetSequence =
    let (|ArraySymSeq|ListSymSeq|SetSymSeq|InfiniteGenSymSeq|FiniteGenSymSeq|OtherSymSeq|) (s:Generic.IEnumerable<Sym<'t>>) =
        match s with
         | :? array<Sym<'t>> -> ArraySymSeq
         | :? list<Sym<'t>> ->  ListSymSeq
         | o when o.GetType().IsGenericType && o.GetType().Name.StartsWith "FSharpSet" && 
            o.GetType().GenericTypeArguments.[0].Name.StartsWith("Sylvester.Sym") -> SetSymSeq
         | :? InfiniteSequenceGen<Sym<'t>> -> InfiniteGenSymSeq
         | :? FiniteSequenceGen<Sym<'t>> -> FiniteGenSymSeq
         | _ -> OtherSymSeq
         
    let (|ArraySeq|ListSeq|SetSeq|InfiniteGenSeq|FiniteGenSeq|OtherSeq|) (s:seq<'t>) =
        match s with
        | :? array<'t> -> ArraySeq
        | :? list<'t> ->  ListSeq
        | o when o.GetType().IsGenericType && o.GetType().Name.StartsWith "FSharpSet" -> SetSeq
        | :? InfiniteSequenceGen<'t> -> InfiniteGenSeq
        | :? FiniteSequenceGen<'t> -> FiniteGenSeq
        | :? seq<Sym<'t>> as se -> 
            match se with
            | ArraySymSeq -> ArraySeq
            | ListSymSeq -> ListSeq
            | SetSymSeq -> SetSeq
            | InfiniteGenSymSeq -> InfiniteGenSeq
            | FiniteGenSymSeq -> FiniteGenSeq
            | _ -> OtherSeq
        | _ -> OtherSeq

    let (|FiniteSeq|_|) (x:Generic.IEnumerable<'t>) =
        match x with
        | ArraySeq
        | ListSeq
        | SetSeq 
        | FiniteGenSeq -> Some x
        | _ -> None

    let (|InfiniteSeq|_|) (x:Generic.IEnumerable<'t>) =
        match x with
        | InfiniteGenSeq -> Some x
        | _ -> None

    let seq_gen f = InfiniteSequenceGen f
    
    let cart_seq (xs:seq<'t>) (ys:seq<'t>) = xs |> Seq.collect (fun x -> ys |> Seq.map (fun y -> x, y))
        
    let rec cart_seq' ss =
        match ss with
        | h::[] ->
            Seq.fold (fun acc elem -> [elem]::acc) [] h
        | h::t ->
            Seq.fold (fun cacc celem ->
                (Seq.fold (fun acc elem -> (elem::celem)::acc) [] h) @ cacc) [] (cart_seq' t)
        | _ -> []

    let cart s = cart_seq s s

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