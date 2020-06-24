namespace Sylvester.Tests.NLU

open Sylvester
open Sylvester.NLU.Wit

module WitTests =

    open System
    open Xunit

    let t = System.Environment.GetEnvironmentVariable("WIT")
    let witClient = new WitClient(t)

    [<Fact>]
    let ``Can create client`` () =
        Assert.NotNull t
        Assert.NotNull witClient
    
    [<Fact>]
    let ``Can parse pred calculus``() =
        let m = witClient.GetMeaning "Prove the theorem not false = true"
       
        do if not (String.IsNullOrEmpty m.Error) then failwithf "The Wit server reported an error: %s." m.Error
        
        let entities = m.Entities |> Seq.choose (fun e -> e.Value |> Some) |> Seq.concat |> Seq.sortBy (fun e -> e.Start)

        Assert.NotEmpty entities

        //Assert.

        

    [<Fact>]
    let ``Can parse``() =
        let a = ExprParser.parse "For "
        Assert.NotNull a