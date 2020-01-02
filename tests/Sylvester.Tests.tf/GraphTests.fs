namespace Sylvester.Tests.tf

open System

open Xunit

open Sylvester
open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10
open Sylvester.Collections
open Sylvester.tf
open Sylvester.Tests

open TensorFlow

type GraphTests() =
    inherit BaseTest()

    [<Fact>]
    let ``Can create graph`` () =
        let g = TensorGraph<dim<6>, dim<1>>("g") |> setDefaultGraph
        checklt(g.Inputs.Length, nine)
        Assert.Equal("g", g.NameScope)
        let m0 = Mat<dim<100>, dim<50>>("m")
        let m1 = Mat<dim<100>, dim<60>>("m")
        Assert.Equal("g/m_0", m0.Name)
        Assert.Equal("g/m_1", m1.Name)
        Assert.True(g.Edges.ContainsKey(m0.Name))
        Assert.Equal(2, g.Nodes.Count)
        Assert.Equal(2, g.Edges.Count)
        
        scope "foo"
        let m2 = Mat<dim<100>, dim<60>>("m")
        let m4 = m1 + m2
        ends()

        Assert.Equal("g/foo/m_0", m2.Name)




        
        

        