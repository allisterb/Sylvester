﻿namespace Sylvester

open System
open System.Collections
open System.Collections.Generic

open FSharp.Quotations
open FSharp.Quotations.Patterns

open Patterns

/// A statement that defines a set using a range, body, and an F# function for computing set membership.
type SetComprehension<'t when 't: equality>([<ReflectedDefinition(true)>] range:Expr<bool>, [<ReflectedDefinition(true)>] body: Expr<'t>, ?test:SetComprehension<'t> ->'t -> bool) = 
    let r = getExprFromReflectedDefinition<bool> range
    let b = getExprFromReflectedDefinition<'t> body
    member val Range = range
    member val Body = body
    member val Body' = b
    member val RangeTest = r
    member val Test = defaultArg test (fun (sc:SetComprehension<'t>) (_:'t) -> failwithf "No test is implemented for the set comprehension %A." sc)
    override x.ToString() = 
        let vars = body |> expand |> get_vars
        let v = if Seq.isEmpty vars then "" else vars.[0].ToString() + "| "
        sprintf "{%s%s:%s}" v (range |> expand |> src) (body |> expand |> src)
    interface IEnumerable<'t> with
        member x.GetEnumerator () = 
            match box x.Body' with
            | :? seq<'t> as e -> e.GetEnumerator()
            | _ -> failwith "Cannot enumerate a set comprehension that is not a sequence."
    interface IEnumerable with
        member x.GetEnumerator () = (x :> IEnumerable<'t>).GetEnumerator () :> IEnumerator

    interface IEquatable<SetComprehension<'t>> with member a.Equals(b) = a.ToString() = b.ToString()
    override a.Equals (_b:obj) = 
            match _b with 
            | :? SetComprehension<'t> as b -> (a :> IEquatable<SetComprehension<'t>>).Equals b
            | _ -> false
    override a.GetHashCode() = (a.ToString()).GetHashCode() 
    new([<ReflectedDefinition(true)>] body: Expr<'t>, test: SetComprehension<'t> -> 't -> bool) = 
        let b = getExprFromReflectedDefinition<'t> body in 
        SetComprehension(true, b, test) 
    
[<AutoOpen>]
module SetComprehension = 
    let set_no_test (sc:SetComprehension<'t>) (_:'t)= failwithf "No test is implemented for the set comprehension %A." sc
    
    let (|ArraySeq|ListSeq|SetSeq|OtherSeq|) (s:IEnumerable<'t>) =
        match s with
        | :? array<'t> -> ArraySeq
        | :? list<'t> ->  ListSeq
        | _ when s.GetType().Name.StartsWith("FSharpSet") -> SetSeq
        | _ -> OtherSeq

    let (|Finite|_|) x =
        match x:IEnumerable<'t> with
        | ArraySeq
        | ListSeq
        | SetSeq -> Some x
        | _ -> None

    let (|FiniteSeq|NonFiniteSeq|) s =
        match s:IEnumerable<'t> with
        | Finite _ -> FiniteSeq
        | _ -> NonFiniteSeq

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