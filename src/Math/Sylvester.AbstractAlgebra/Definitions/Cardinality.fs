namespace Sylvester 

open FSharp.Quotations

type Cardinality =
| Finite of DelayedEval<real>
| Aleph0
| Aleph1
    with    
        member x.Measure() = 
            match x with
            | Finite m -> m.Force()
            | _ -> failwith "This set does not have finite cardinality."

type ICardinality = abstract Cardinality: Cardinality

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
  
    let measure (s:ISet<'t>) = let c = (card s) in c.Measure()

    let failIfCardinalityNotKnown(s:ISet<'t>) =
        if not (s :? ICardinality) then failwith "The cardinality of this set is not known."

    let failIfCardinalityNotKnown'(l:ISet<'t>) (r:ISet<'t>) =
        if not (l :? ICardinality) then failwith "The cardinality of the LHS set is not known."
        if not (r :? ICardinality) then failwith "The cardinality of the RHS set is not known."

    let (|Countable|Uncountable|) =
        function
        | Finite _ 
        | Aleph0 -> Countable
        | Aleph1 -> Uncountable

    let failIfNotCountable(s:ISet<'t>) =
        match card s with
        | Countable -> ()
        | Uncountable -> failwith "This set is not countable."

    let finite_seq (s:seq<'t>) =
        let _set = Seq s in
        {
            new ISet<'t> with
                member x.Set = _set
                member x.Equals b = _set.Equals b
            interface ICardinality with
                member x.Cardinality  = lazy(_set |> Seq.length |> float) |> Finite
        }

    let infinite_seq g = 
        let _set = g |> Seq.initInfinite |> Set.fromSeq in
        {
            new ISet<'t> with
                member x.Set = _set
                member x.Equals b = _set.Equals b
            interface ICardinality with
                member x.Cardinality  = Aleph0
        }
   
    let infinite_seq2 g = 
        let _set = g |> Seq.initInfinite |> cart |> Set.fromSeq in
        {
            new ISet<'t * 't> with
                member x.Set = _set
                member x.Equals b = _set.Equals b
            interface ICardinality with
                member x.Cardinality  = Aleph0
        }


    let finite_set (range:Expr<bool>) (body:Expr<'t>) (size:int) =
        let _set = set range body in
        {
            new ISet<'t> with
                member x.Set = _set
                member x.Equals b = _set.Equals b
            interface ICardinality with
                member x.Cardinality  = lazy(size |> float) |> Finite
        }

    let (<*>) (l:ISet<'t>) (r:ISet<'t>) =
        let resc = 
            match card l, card r with
            | Finite a, Finite b -> Finite (lazy(a.Force() * b.Force()))
            | Aleph0, Aleph0 -> Aleph0
            | _ -> failwith "The Cartesian product is only defined between 2 countable sets."
        let resp = Seq (cart2 l.Set r.Set) in
        {
            new ISet<'t*'t> with
                member x.Set = resp 
                member x.Equals b = resp.Equals b
            interface ICardinality with
                member x.Cardinality  = resc
        }
        