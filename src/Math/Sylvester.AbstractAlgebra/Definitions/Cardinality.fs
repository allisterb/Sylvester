namespace Sylvester 

open FSharp.Quotations

type CardinalNumber =
| Finite of DelayedEval<real>
| Aleph0
| Aleph1
    with    
        member x.Measure() = 
            match x with
            | Finite m -> m.Force()
            | _ -> failwith "This set does not have finite cardinality."

type ICardinality = abstract Cardinality: CardinalNumber

[<AutoOpen>]
module Cardinality =
    let card (s:ISet<'t>) =
        match s with
        | :? ICardinality as c -> c.Cardinality
        | e when e.Set = Empty -> lazy(float 0) |> Finite
        | _ -> 
            match s.Set with
            | Empty -> lazy(float 0) |> Finite
            | Seq _ -> lazy(s.Set |> Seq.length |> float) |> Finite
            | _ -> failwith "Cannot automatically determine the cardinality of a set comprehension."
  
    let with_card (c:CardinalNumber) (s:ISet<'t>) =
        {
            new ISet<'t> with
                member x.Set = s.Set
                member x.Equals b = s.Equals b
            interface ICardinality with
                member x.Cardinality = c
        }

    let measure (s:ISet<'t>) = let c = (card s) in c.Measure()

    let (|Countable|Uncountable|) =
        function
        | Finite _ 
        | Aleph0 -> Countable
        | Aleph1 -> Uncountable

    let failIfCardinalityNotKnown(s:ISet<'t>) =
        if not (s :? ICardinality) then failwith "The cardinality of this set is not known."

    let failIfCardinalityNotKnown'(l:ISet<'t>) (r:ISet<'t>) =
        if not (l :? ICardinality) then failwith "The cardinality of the LHS set is not known."
        if not (r :? ICardinality) then failwith "The cardinality of the RHS set is not known."

    let failIfNotCountable(s:ISet<'t>) =
        match card s with
        | Countable -> ()
        | Uncountable -> failwith "This set is not countable."

    let finite_seq s = s |> Set.fromSeq |> with_card (Finite(lazy(s |> Seq.length |> float)))

    let infinite_seq g = g |> Seq.initInfinite |> Set.fromSeq |> with_card Aleph0
        
    let infinite_seq2 g = g |> Seq.initInfinite |> cart |> Set.fromSeq |> with_card Aleph0
        
    let infinite_seqp2 g = g |> Seq.initInfinite |> pairwise |> Set.fromSeq |> with_card Aleph0

    let infinite_seqp3 g = g |> Seq.initInfinite |> triplewise |> Set.fromSeq |> with_card Aleph0

    let infinite_seqp4 g = g |> Seq.initInfinite |> quadwise |> Set.fromSeq |> with_card Aleph0

    let infinite_seqp5 g = g |> Seq.initInfinite |> quintwise |> Set.fromSeq |> with_card Aleph0

    let infinite_seq' (f: Expr<int ->'t ->'t>) =
        Seq.initInfinite(fun i -> 
                    let b = (body f).Substitute(fun v -> if v.Name = "n" then Some(Expr.Value i) else None)
                    SymExpr(<@ (%%b:'t) @>)) 
                    |> Set.fromSeq

    let finite_set (range:Expr<bool>) (body:Expr<'t>) (size:int) =
        let _set = set range body in
        {
            new ISet<'t> with
                member x.Set = _set
                member x.Equals b = _set.Equals b
            interface ICardinality with
                member x.Cardinality  = lazy(size |> float) |> Finite
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
            | Finite a, Finite b -> Finite (lazy(a.Force() * b.Force()))
            | Aleph0, Aleph0 -> Aleph0
            | Finite a, Aleph0 -> Finite a
            | Aleph0, Finite b -> Finite b
            | _ -> failwith "The Cartesian product is only defined between 2 countable sets."
        Seq (cart2 l.Set r.Set) |>*| resc
        