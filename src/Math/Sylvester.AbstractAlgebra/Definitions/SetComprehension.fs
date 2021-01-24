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

    let cart (source: seq<'a>) =
        seq { 
            use e = source.GetEnumerator() 
            let prev = new System.Collections.Generic.List<'a ref>()
           
            while e.MoveNext() do
                let i = ref e.Current
                prev.Add i
                yield! seq {for p in prev do yield (!i, !p)}
                yield! seq {for p in prev do yield (!p, !i)}                 
        }

    let cart2 (source1: seq<'a>) (source2:seq<'b>) =
        seq { 
            use e1 = source1.GetEnumerator()
            use e2 = source2.GetEnumerator()
            let prev = new System.Collections.Generic.List<'a ref *'b ref>()
           
            while (e1.MoveNext() && e2.MoveNext()) do
                let i = ref e1.Current
                let j = ref e2.Current
                prev.Add ((i, j))
                yield! seq {for p in prev do yield (!i, !(snd p))}
                yield! seq {for p in prev do yield (!(fst p), !j)} 
        }

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