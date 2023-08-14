namespace Sylvester 

open FSharp.Reflection

[<StructuredFormatDisplay("{Display}")>]
type CardinalNumber =
| Finite of DelayedEval<int>
| Aleph of int
| Unknown
with    
    member x.Measure() :real = 
        match x with
        | Finite m -> real(m.Force())
        | _ -> failwith "This set does not have finite cardinality."
    
    member x.Display =
        match x with
        | Finite m -> if m.IsValueCreated then sprintf "Finite %A" m.Value else "Finite"
        | Aleph a -> sprintf "Aleph %A" a
        | Unknown -> "Unknown"
    
    override x.ToString() = x.Display
    static member (+) (l:CardinalNumber, r:CardinalNumber) =
        match l, r with
        | Finite a, Finite b -> Finite(lazy(a.Force() + b.Force()))
        | Finite _, Aleph n -> Aleph n
        | Aleph n, Finite _ -> Aleph n
        | Aleph m, Aleph n -> if m >= n then Aleph m else Aleph n
        | _, _ -> Unknown
    static member (-) (l:CardinalNumber, r:CardinalNumber) =
        match l, r with
        | Finite a, Finite b -> if a.Force() >= b.Force() then Finite(lazy(a.Value + b.Value)) else failwith "Cannot subtract a higher finite cardinal number from a lower finite cardinal number."
        | Finite _, Aleph n -> failwith "Cannot subtract an infinite cardinal number from a finite cardinal number"
        | Aleph n, Finite _ -> Aleph n
        | Aleph m, Aleph n  -> if m > n then Aleph m else failwith "Cannot subtract a higher or equal infinite cardinal number from a lower infinite cardinal number."
        | _, _ -> Unknown
    static member (*) (l:CardinalNumber, r:CardinalNumber) =
        match l, r with
        | Finite a, Finite b -> lazy(System.Math.Max(a.Force(), a.Force())) |> Finite
        | Finite _, Aleph n -> Aleph n
        | Aleph m, Finite _ -> Aleph m
        | Aleph m, Aleph n -> System.Math.Max(m, n) |> Aleph
        | _, _ -> Unknown
    static member (/) (l:CardinalNumber, r:CardinalNumber) =
        match l, r with
        | Finite a, Finite b -> lazy(System.Math.Min(a.Force(), a.Force())) |> Finite
        | Finite m, Aleph _ -> Finite m
        | Aleph _, Finite n -> Finite n
        | Aleph m, Aleph n -> System.Math.Min(m, n) |> Aleph
        | _, _ -> Unknown

type ICardinality =  
    abstract member Cardinality:CardinalNumber

[<AutoOpen>]
module Cardinality =
    let default_card<'t> =
        let _default_card (t:System.Type) = 
            match t.Name with
            | "Int8"
            | "UInt8"
            | "Int16" 
            | "UInt16"
            | "Int32"
            | "UInt32"
            | "Int64"
            | "UInt64"
            | "Rational"
            | "Natural"
            | "String" -> Some <| Aleph 0
            | "Single"
            | "Double" 
            | "Complex" -> Some <| Aleph 1
            | _ -> Some <| Unknown //
        let t = typeof<'t>
        if FSharpType.IsTuple t then
            let a = FSharpType.GetTupleElements t |> Array.map _default_card
            do if a |> Array.exists(fun e -> Option.isNone e) then failwithf "Unable to automatically determine cardinality for one of the elements of cardinal type %s." t.Name
            a |> Array.map Option.get |> Array.reduce(+)
        else
            match _default_card t with
            | Some c -> c
            | None -> failwithf "Unable to automatically determine cardinality of type %s." t.Name