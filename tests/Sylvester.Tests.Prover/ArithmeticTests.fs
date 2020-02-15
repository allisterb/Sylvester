namespace Sylvester.Tests.Prover

[<ReflectedDefinition;AutoOpen>]
module private Formulae =
    let f x = 2 * x + 8 
    let g x = 2 * x + 3 + 5

module Arithmetic =

    open System
    open System.Linq

    open Xunit

    open Sylvester


    [<Fact>]
    let ``Can reduce`` () =
        let f1,f2 = F f, F g
        let e = reduceConstantIntOperands f2.Expr
        Assert.True(e <=> f1)
    
        
