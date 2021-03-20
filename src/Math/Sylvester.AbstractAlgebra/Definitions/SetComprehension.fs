namespace Sylvester

open System
open System.Collections.Generic

open FSharp.Quotations

[<CustomEquality; CustomComparison>]
type SymExpr<'t> = SymExpr of Expr<'t> with
     member x.Expr = let (SymExpr e) = x in e
     interface IEquatable<SymExpr<'t>> with member a.Equals b = a.Expr.ToString() = b.Expr.ToString()
     interface IComparable<SymExpr<'t>> with
         member a.CompareTo b = if a.Expr = b.Expr then 0 else if b.Expr.ToString().Length > a.Expr.ToString().Length then -1 else 1
     interface IComparable with
         member a.CompareTo b = 
             match b with
             | :? SymExpr<'t> as e -> (a :> IComparable<SymExpr<'t>>).CompareTo e
             | _ -> failwith "This object is not an EExpr."
     override a.GetHashCode() = (a.Expr.ToString()).GetHashCode()
     override a.Equals (_b:obj) = 
             match _b with 
             | :? SymExpr<'t> as e -> (a :> IEquatable<SymExpr<'t>>).Equals e
             | _ -> false
     override x.ToString() = src (x.Expr)

type any = SymExpr<obj>

/// A statement that formally defines a set using a range, body, and an optional F# function for computing set membership.
type SetComprehension<'t when 't: equality>(range:Expr<bool>, body: Expr<'t>, ?hasElement:SetComprehension<'t> ->'t -> bool) = 
    member val Range = expand range
    member val internal Range' = range
    member val Body = expand body
    member val internal Body' = body
    member val HasElement = defaultArg hasElement (fun (sc:SetComprehension<'t>) (_:'t) -> failwithf "No set membership function is defined for the set comprehension %A." sc)
    override x.ToString() = 
        let vars = body |> get_vars
        let v = if Seq.isEmpty vars then "" else vars.[0].ToString() + "|"
        sprintf "{%s%s:%s}" v (src range) (src body)
    interface IEquatable<SetComprehension<'t>> with member a.Equals(b) = a.ToString() = b.ToString()
    override a.GetHashCode() = (a.ToString()).GetHashCode()
    override a.Equals (_b:obj) = 
            match _b with 
            | :? SetComprehension<'t> as b -> (a :> IEquatable<SetComprehension<'t>>).Equals b
            | _ -> false
    new(body: Expr<'t>, test: SetComprehension<'t> -> 't -> bool) = 
        let b = getExprFromReflectedDefinition<'t> body in 
        SetComprehension(<@ true @>, body, test) 
    
[<AutoOpen>]
module SetComprehension = 
    let (|ArraySeq|ListSeq|SetSeq|SymExprSeq|OtherSeq|) (s:IEnumerable<'t>) =
        match s with
        | :? array<'t> -> ArraySeq
        | :? list<'t> ->  ListSeq
        | _ when s.GetType().Name.StartsWith("FSharpSet") -> SetSeq
        | :? seq<SymExpr<'t>> -> SymExprSeq
        | _ -> OtherSeq

    let (|FiniteSeq|_|) x =
        match x:IEnumerable<'t> with
        | ArraySeq
        | ListSeq
        | SetSeq -> Some x
        | _ -> None

    let cart_seq xs ys = 
        xs |> Seq.collect (fun x -> ys |> Seq.map (fun y -> x, y))
    
    let rec cart_seq' ss =
        match ss with
        | h::[] ->
            Seq.fold (fun acc elem -> [elem]::acc) [] h
        | h::t ->
            Seq.fold (fun cacc celem ->
                (Seq.fold (fun acc elem -> (elem::celem)::acc) [] h) @ cacc
                ) [] (cart_seq' t)
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