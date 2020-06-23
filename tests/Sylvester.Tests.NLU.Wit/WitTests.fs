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
        let m = witClient.GetMeaning "Socrates is a man and all men are mortal then Socrates is mortal"
       
        do if not (String.IsNullOrEmpty m.Error) then failwithf "The Wit server reported an error: %s." m.Error
        
        let entities = m.Entities |> Seq.choose (fun e -> e.Value |> Some) |> Seq.concat |> Seq.sortBy (fun e -> e.Start)

        Assert.NotEmpty entities

        //Assert.

        

    [<Fact>]
    let ``Can parse``() =
        let a = ExprParser.parse "p and q and (q or p)"
        Assert.NotNull a