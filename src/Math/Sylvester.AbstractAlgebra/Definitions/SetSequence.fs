namespace Sylvester

open System.Collections.Generic

[<AutoOpen>]
module SetSequence =
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

