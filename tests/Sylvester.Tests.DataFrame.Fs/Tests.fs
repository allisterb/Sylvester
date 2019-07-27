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
        let file = new CsvFile("https://raw.githubusercontent.com/matplotlib/sample_data/master/msft.csv")
        file.[0].Type <- typeof<DateTime>
        for j in 1..file.Fields.Count - 1 do file.[j].Type <- typeof<float> 
        let f = new Frame(file = file);
        let d = f?Date;
        Assert.NotNull(d);
        Assert.NotNull(f.[0]);
        ()

