namespace Sylvester

open System
open System.Collections.Generic

open Sylvester.NLU.Wit

module NLU =
    
    let witClient = new WitClient(System.Environment.GetEnvironmentVariable("WIT"))
    
    let getMeaning s = 
        let m = witClient.GetMeaning s
        do if not (String.IsNullOrEmpty m.Error) then failwithf "The Wit server reported an error: %s." m.Error
        
        let entities = m.Entities |> Seq.choose (fun e -> e.Value |> Some) |> Seq.concat

        Assert.NotEmpty entities

