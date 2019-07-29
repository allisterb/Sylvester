namespace Sylvester.Tests

open System;
open System.Linq;
open Xunit;

open Sylvester;
open Sylvester.Data;
open FSharp.Interop.Dynamic;

module FsDataFrameTests = 

    [<Fact>]
    let ``Can construct data frame`` () =
        let msft = new CsvFile("https://raw.githubusercontent.com/matplotlib/sample_data/master/msft.csv")
        msft.[0].Type <- typeof<DateTime>
        for j in 1..msft.Fields.Count - 1 do msft.[j].Type <- typeof<float> 
        msft.Last().Label <- "AdjClose"
        let f = new Frame(msft);
        let d = f?Date;
        Assert.NotNull(d);
        Assert.IsType<DateTime>(f.[0].[0]) |> ignore;
        Assert.IsType<double>(f.[0].[1]) |> ignore;
        let q1 = query {for r in f do select r?Volume}
        Assert.NotEmpty(q1);
        f?Foo<-Sn<float>.Rnd(f.Length);
        Assert.NotEmpty(f?Foo);

    [<Fact>]
    let ``Can query data frame``() =
        //Use the Titanic CSV dataset 
        let titanic = new CsvFile("https://raw.githubusercontent.com/datasciencedojo/datasets/master/titanic.csv")
        titanic.["PassengerId"].Type <- typeof<int>
        titanic.["Survived"].Type <- typeof<int>
        let dt = new Frame(titanic)
        dt?Survived2<-new Sn<bool>(dt.Select(fun r -> if r?Survived = 1 then true else false))
        Assert.NotEmpty(dt?Survived2)
        Assert.NotNull(dt.[0]?Survived2)

    [<Fact>]
    let ``Can construct data frame window``() =
        let titanic = new CsvFile("https://raw.githubusercontent.com/datasciencedojo/datasets/master/titanic.csv")
        titanic.["PassengerId"].Type <- typeof<int>
        titanic.["Survived"].Type <- typeof<int>
        let dt = new Frame(titanic)
        let w = new FrameW<string>(dt, fun s -> 
            let name:Ss = dt?Name in Array.IndexOf<string>(name.Data, s))
        Assert.NotNull(w.["Braund, Mr. Owen Harris"])

        let w2 = dt.SWnd(dt?Name)
        Assert.NotNull(w2.["Braund, Mr. Owen Harris"])

        Assert.NotEmpty(w2.SelC("PassengerId"))
        let dr1 = dt.SelC(dt.["PassengerId"], dt.["Name"])
        Assert.NotEmpty(dr1)
  
