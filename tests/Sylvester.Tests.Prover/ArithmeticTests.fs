namespace Sylvester.Tests.Prover

[<ReflectedDefinition;AutoOpen>]
module private Formulae =
    let f x = 2 * x + 8 
    let g x = 2 * x + 3 + 5
    let inline s (x) = Seq.sum (seq{1..x})

module Arithmetic =

    open System
    open System.Linq

    open Xunit

    open Sylvester
    [<Fact>]
    let ``Can sum`` () =
        let f1,f2,f3 = F f, F g, F s
        let e = replaceSum f3.Expr
        Assert.True(e <=> f1)

    [<Fact>]
    let ``Can reduce`` () =
        let f1,f2 = F f, F g
        let fsum = F 
        let e = reduceConstantIntOperands f2.Expr
        Assert.True(e <=> f1)
    
        
