namespace Sylvester

open System
open System.Collections.Generic

open Sylvester.NLU

module NLU =

    type Intent = 
    | Theorem of float32 * Entity list 
    | ApplyRule

    and Entity = Entity of string * string * int

    let witClient = new Wit.WitClient() |> init

    let getIntent (s:string) =
        match !> witClient.GetMeaning s with
        | Success m when m.Intents.Count = 1 && m.Intents.[0].Confidence >= 0.9f  ->
            let intent = m.Intents.[0] 
            match intent.Name with
            | "theorem" -> Theorem(intent.Confidence, []) |> Success
            | "apply_rule" -> ApplyRule |> Success
            | _ -> exn "Unknown intent" |> Failure           
        | Success m when m.Intents.Count = 1 && m.Intents.[0].Confidence < 0.9f -> 
            let intent = m.Intents.[0]
            sprintf "The confidence of intent %s : %f is less than 0.9." intent.Name intent.Confidence |> exn |> Failure
        | Success m -> sprintf "Unknown meaning %A." m |> exn |> Failure
        | Failure f -> Failure f

  
    
    
