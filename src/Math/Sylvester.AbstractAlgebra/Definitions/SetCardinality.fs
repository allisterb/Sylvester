namespace Sylvester 

type CardinalNumber =
| Finite of DelayedEval<int>
| Aleph of int
with    
    member x.Measure() :real = 
        match x with
        | Finite m -> float(m.Force())
        | _ -> failwith "This set does not have finite cardinality."
    static member (+) (l:CardinalNumber, r:CardinalNumber) =
        match l, r with
        | Finite a, Finite b -> Finite(lazy(a.Force() + b.Force()))
        | Finite _, Aleph n -> Aleph n
        | Aleph n, Finite _ -> Aleph n
        | Aleph m, Aleph n -> if m >= n then Aleph m else Aleph n
    static member (-) (l:CardinalNumber, r:CardinalNumber) =
        match l, r with
        | Finite a, Finite b -> if a.Force() >= b.Force() then Finite(lazy(a.Value + b.Value)) else failwith "Cannot subtract a higher finite cardinal number from a lower finite cardinal number."
        | Finite _, Aleph n -> failwith "Cannot subtract an infinite cardinal number from a finite cardinal number"
        | Aleph n, Finite _ -> Aleph n
        | Aleph m, Aleph n  -> if m > n then Aleph m else failwith "Cannot subtract a higher or equal infinite cardinal number from a lower infinite cardinal number."
    static member (*) (l:CardinalNumber, r:CardinalNumber) =
        match l, r with
        | Finite a, Finite b -> lazy(System.Math.Max(a.Force(), a.Force())) |> Finite
        | Finite _, Aleph n -> Aleph n
        | Aleph m, Finite _ -> Aleph m
        | Aleph m, Aleph n -> System.Math.Max(m, n) |> Aleph
    static member (/) (l:CardinalNumber, r:CardinalNumber) =
        match l, r with
        | Finite a, Finite b -> lazy(System.Math.Min(a.Force(), a.Force())) |> Finite
        | Finite m, Aleph _ -> Finite m
        | Aleph _, Finite n -> Finite n
        | Aleph m, Aleph n -> System.Math.Min(m, n) |> Aleph

type ICardinality =  
    abstract member Cardinality:CardinalNumber

[<AutoOpen>]
module SetCardinality =
    let finite_card f = Finite f

    let countable_infinite_card = Aleph 0
    
    let infinite_card n = Aleph n

    let default_card<'t> =
        match typeof<'t>.Name with
        | "Int32" -> Aleph 0
        | _ -> failwithf "Unable to automatically determine cardinality of type %s" typeof<'t>.Name