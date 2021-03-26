namespace Sylvester 

open System.Collections
open FSharp.Quotations

type CardinalNumber =
| Countable of Countable
| Aleph of int
with    
    member x.Validate = 
        match x with 
        | Aleph a -> if a <= 0 then failwith "This parameter must be > 0." 
        | _ -> ()
    member x.Measure() = 
        match x with
        | Countable (Finite m) -> float(m.Force())
        | _ -> failwith "This set does not have finite cardinality."
and Countable =
| Finite of DelayedEval<int>
| Aleph0

type ICardinality = abstract Cardinality: CardinalNumber

type ICountable<'t when 't : equality> =
    inherit ISet<'t>
    inherit ICardinality
    inherit Generic.IEnumerable<'t>
    abstract Count:Countable

[<AutoOpen>]
module Cardinality =
    let card (s:ISet<'t>) =
        match s with
        | :? ICardinality as c -> c.Cardinality
        | e when e.Set = Empty -> lazy 0 |> Finite |> Countable
        | _ -> 
            match s.Set with
            | Empty -> lazy 0 |> Finite |> Countable
            | Seq _ -> lazy(s.Set |> Seq.length) |> Finite |> Countable
            | _ -> failwith "Cannot automatically determine the cardinality of a set comprehension."
  
    let measure (s:ISet<'t>) = let c = (card s) in c.Measure()

    let failIfCardinalityNotKnown(s:ISet<'t>) =
        if not (s :? ICardinality) then failwith "The cardinality of this set is not known."

    let failIfCardinalityNotKnown'(l:ISet<'t>) (r:ISet<'t>) =
        if not (l :? ICardinality) then failwith "The cardinality of the LHS set is not known."
        if not (r :? ICardinality) then failwith "The cardinality of the RHS set is not known."

    let finite_card f = Countable(Finite f)

    let countable_infinite_card = Countable Aleph0

    let aleph0 = countable_infinite_card

    let with_card (c:CardinalNumber) (s:ISet<'t>) =
        {
            new ISet<'t> with
                member x.Set = s.Set
                member x.Equals b = s.Equals b
            interface ICardinality with
                member x.Cardinality = c
        }

    let countable_set (c:Countable) (s:ISet<'t>)  =
        {
            new ICountable<'t> with
                member x.Set = s.Set
                member x.Equals b = s.Equals b
                member x.Cardinality = Countable c
                member x.Count = c    
                member x.GetEnumerator():Generic.IEnumerator<'t> = (x.Set :> Generic.IEnumerable<'t>).GetEnumerator()
                member x.GetEnumerator():IEnumerator = (x.Set :> IEnumerable).GetEnumerator()
        }

    let countable_infinite_set (s:ISet<'t>) = countable_set Aleph0 s

    let finite_seq s = s |> Set.fromSeq |> countable_set (lazy(s |> Seq.length) |> Finite)

    let infinite_seq g = g |> Seq.initInfinite |> Set.fromSeq |> countable_infinite_set
        
    let infinite_seq2 g = g |> Seq.initInfinite |> cart |> Set.fromSeq |> countable_infinite_set
        
    let infinite_seqp2 g = g |> Seq.initInfinite |> pairwise |> Set.fromSeq |> countable_infinite_set

    let infinite_seqp3 g = g |> Seq.initInfinite |> triplewise |> Set.fromSeq |> countable_infinite_set

    let infinite_seqp4 g = g |> Seq.initInfinite |> quadwise |> Set.fromSeq |> countable_infinite_set

    let infinite_seqp5 g = g |> Seq.initInfinite |> quintwise |> Set.fromSeq |> countable_infinite_set

    let infinite_seq' (f: Expr<int ->'t ->'t>) =
        let vf = get_vars f |> List.head
        Seq.initInfinite(fun i -> 
                    let b = (body f).Substitute(fun v -> if v.Name = vf.Name && v.Type = vf.Type then Some(Expr.Value i) else None)
                    SymExpr(<@ (%%b:'t) @>)) 
                    |> Set.fromSeq
        |> countable_infinite_set

    let finite_set (range:Expr<bool>) (body:Expr<'t>) (size:int) =
        let _set = set range body in
        {
            new ISet<'t> with
                member x.Set = _set
                member x.Equals b = _set.Equals b
            interface ICardinality with
                member x.Cardinality  = lazy(size) |> Finite |> Countable
        }

    let infinite_set (range:Expr<bool>) (body:Expr<'t>) (c:CardinalNumber)=
        let _set = set range body in
        {
            new ISet<'t> with
                member x.Set = _set
                member x.Equals b = _set.Equals b
            interface ICardinality with
                member x.Cardinality = c
        }

    let (|>*|) (s:ISet<'t>) (c:CardinalNumber) = s |> with_card c

    let (<*>) (l:ISet<'t>) (r:ISet<'t>) =
        let resc = 
            match card l, card r with
            | Countable x, Countable y ->
                match x, y with
                | Finite a, Finite b -> Finite (lazy(a.Force() * b.Force()))
                | Aleph0, Aleph0 -> Aleph0
                | Finite a, Aleph0 -> Finite a
                | Aleph0, Finite b -> Finite b
            | _ -> failwith "The Cartesian product is only defined between 2 countable sets."
        Seq (cart2 l.Set r.Set) |> countable_set resc 
        
        