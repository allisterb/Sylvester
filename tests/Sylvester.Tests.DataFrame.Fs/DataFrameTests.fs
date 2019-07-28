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
        let gg = f.[55];
        Assert.IsType<DateTime>(f.[0].[0]) |> ignore;
        Assert.IsType<double>(f.[0].[1]) |> ignore;
        let qi = query {for r in f do select r?Volume}
        
        ()

