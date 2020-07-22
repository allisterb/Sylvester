namespace Sylvester

open System
open System.Collections
open System.Collections.Generic

open FSharp.Quotations
open FSharp.Quotations.Patterns

/// A statement that formally defines a set using a range, body, and an F# function for computing set membership.
type SetComprehension<'t when 't: equality>([<ReflectedDefinition(true)>] range:Expr<bool>, [<ReflectedDefinition(true)>] body: Expr<'t>, test:SetComprehension<'t> ->'t -> bool) = 
    let r = getExprFromReflectedDefinition<bool> range
    let b = getExprFromReflectedDefinition<'t> body
    member val Range = range
    member val Body = body
    member val Body' = b
    member val RangeTest = r
    member val Test = test 
    override x.ToString() = 
        let vars = body |> expand |> get_vars
        let v = if Seq.isEmpty vars then "" else vars.[0].ToString() + "| "
        sprintf "{%s%s:%s}" v (range |> expand |> src) (body |> expand |> src)
    interface IEquatable<SetComprehension<'t>> with member a.Equals(b) = a.ToString() = b.ToString()
    override a.Equals (_b:obj) = 
            match _b with 
            | :? SetComprehension<'t> as b -> (a :> IEquatable<SetComprehension<'t>>).Equals b
            | _ -> false
    override a.GetHashCode() = (a.ToString()).GetHashCode() 
    new(body: 't, test: SetComprehension<'t> -> 't -> bool) = SetComprehension(true, body, test) 

/// A generator defines a set that is a sequence together with a predicate for testing set membership.
type SetGenerator<'t when 't: equality>([<ReflectedDefinition(true)>] s:Expr<seq<'t>>, [<ReflectedDefinition(true)>] predicate:Expr<'t -> bool>) = 
    let sv = match s with | WithValue(v, _, _) -> v | _ -> failwith "Unexpected expression."
    let pv = match predicate with | WithValue(v, _, _) -> v | _ -> failwith "Unexpected expression."
    member val Test = pv :?> ('t -> bool)
    member val Predicate = predicate
    member val Seq = sv :?> (seq<'t>)
    member val Body = s
    override x.ToString() = x.Seq.ToString()
    member x.HasElement elem = x.Test elem
    interface IEnumerable<'t> with
        member x.GetEnumerator () = x.Seq.GetEnumerator() 
    interface IEnumerable with
        member x.GetEnumerator () = (x :> IEnumerable<'t>).GetEnumerator () :> IEnumerator

/// A generator defines a set that is a sequence together with a predicate for testing set membership.
type Gen<'t when 't: equality> = SetGenerator<'t>

[<AutoOpen>]
module SetComprehension = 
    let seq_body<'a, 't> (r:'a) = Unchecked.defaultof<'t>

    let (|ArraySeq|ListSeq|SetSeq|GeneratorSeq|OtherSeq|) (s:IEnumerable<'t>) =
        match s with
        | :? array<'t> -> ArraySeq
        | :? list<'t> ->  ListSeq
        | _ when s.GetType().Name.StartsWith("FSharpSet") -> SetSeq
        | :? SetGenerator<'t> -> GeneratorSeq
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

    let (|Generator|_|) x =
        match x:IEnumerable<'t> with
        | :? SetGenerator<'t> as s -> Some s
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