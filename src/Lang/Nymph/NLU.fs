namespace Sylvester

open System
open System.Collections.Generic

open Sylvester.NLU

module Nymph =
    type Intent = 
        | Theorem of float32 * Entity list 
        | ApplyRule of float32 * Entity list
    with 
        member x.Entities = 
            match x with
            | Theorem (_, e) 
            | ApplyRule(_, e) -> e 
    
    and Entity = Entity of string * string * string * float32 * int with
        member x.Name =
            let (Entity(n, v, r, c, s)) = x in n
        member x.Confidence =
            let (Entity(n, v, r, c, s)) = x in c
        static member fromWit (e:Wit.Entity) = Entity(e.Name, e.Value, e.Role, e.Confidence, e.Start)
    
    let (|WitMathExpression|_|):Wit.Entity -> Entity option =
        function
        | e when e.Name = "wit$math_expression" && e.Confidence > 0.9f -> e |> Entity.fromWit |> Some
        | _ -> None
    
    let (|MathEntity|_|):Entity->Entity option =
        function
        | e when e.Name = "wit$math_expression" -> Some e
        | _ -> None

    let witClient = new Wit.WitClient() |> init

    let getIntent (s:string) =
        match !> witClient.GetMeaning s with
        | Success m when m.Intents.Count = 1 && m.Intents.[0].Confidence >= 0.9f  ->
            let intent = m.Intents.[0] 
            let entities =
                m.Entities.Values
                |> Seq.concat
                |> Seq.choose(fun e -> match e with | WitMathExpression e -> Some e | _ -> None) 
                |> List.ofSeq
            match intent.Name with
            | "theorem" -> Theorem(intent.Confidence, entities) |> Success
            | "apply_rule" -> ApplyRule(intent.Confidence, []) |> Success
            | _ -> exn "Unknown intent" |> Failure           
        | Success m when m.Intents.Count = 1 && m.Intents.[0].Confidence < 0.9f -> 
            let intent = m.Intents.[0]
            sprintf "The confidence of intent %s : %f is less than 0.9." intent.Name intent.Confidence |> exn |> Failure
        | Success m -> sprintf "Unknown meaning %A." m |> exn |> Failure
        | Failure f -> Failure f

  
    
    
