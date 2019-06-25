namespace Sylvester.Tests.Tensors

module FileDataTests = 
    
    open System.IO
    open Xunit

    open Sylvester.Arithmetic
    open Sylvester.Arithmetic.N10
    open Sylvester.Arithmetic.Collections
    open Sylvester.Tensors
    open Sylvester.Tensors.IO

    [<Fact>]
    let ``Can read single column from CSV ``() = 
        let fn = Path.Combine("Data", "iris.data")
        let data = FileData.readCsvSingleCol<single> fn false 0
        Assert.NotEmpty(data)
        let arr = Seq.toArray data
        Assert.Equal(5.1f, arr.[0])


        let data2 = FileData.readCsvSingleCol<single> fn true 0
        Assert.NotEmpty(data2)
        let arr2 = Seq.toArray data2
        Assert.Equal(4.9f, arr2.[0])
        
        let data3 = FileData.readCsvSingleCol<single> fn false 3
        Assert.NotEmpty(data3)
        let arr3 = Seq.toArray data3
        Assert.Equal(0.2f, arr3.[0])
        
    [<Fact>]
    let ``Can read multiple columns from CSV ``() = 
        let fn = Path.Combine("Data", "iris.data")
        let _data = FileData.readCsvCols<single> fn false [0; 1; 2] |> Seq.toArray
        Assert.NotEmpty(_data)
        let data = Array2D.init _data.Length 3 (fun i j -> _data.[i].[j])
        Assert.NotEmpty(data)

    
        
