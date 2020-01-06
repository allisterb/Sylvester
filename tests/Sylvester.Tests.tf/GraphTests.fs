namespace Sylvester.Tests.tf

open System
open System.Linq

open Xunit

open Sylvester
open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10
open Sylvester.Collections
open Sylvester.tf
open Sylvester.Tests

open Google.Protobuf
open Google.Protobuf.Collections

open TensorFlow


type GraphTests() =
    inherit BaseTest()

    [<Fact>]
    let ``Can create graph`` () =
        let g = TensorGraph<dim<6>, dim<1>>("g")
        checklt(g.Inputs.Length, nine)
        Assert.Equal("g", g.NameScope)
        defaultGraph <- g
        let m0 = Mat<dim<100>, dim<60>>("m")
        let m1 = Mat<dim<100>, dim<60>>("m")
        Assert.Equal("g/m_0", m0.Name)
        Assert.Equal("g/m_1", m1.Name)
        Assert.True(g.Edges.ContainsKey(m0.Name))
        Assert.Equal(2, g.Nodes.Count)
        Assert.Equal(2, g.Edges.Count)
        Assert.True(g.Nodes.Values.All(fun n -> n.Name.StartsWith "g/"))
        
    [<Fact>]
    let ``Can create graph scope``() =
        defaultGraph <- TensorGraph<dim<6>, dim<1>>()
        let m0 = Mat<dim<100>, dim<60>>("m")
        let m1 = Mat<dim<100>, dim<60>>("m")
        Assert.Equal("m_0", m0.Name)
        Assert.Equal("m_1", m1.Name)
        let m2 = 
            use x = subscope "x"
            Mat<dim<100>, dim<60>>("m")
        Assert.Equal("x/m_0", m2.Name)
        use x2 = subscope "x2"
        use x3 = subscope "x3"
        let m3 = Mat<dim<20>, dim<5>>("m")
        Assert.Equal("x2/x3/m_0", m3.Name)
        ends x3
        let m4 = Mat<dim<20>, dim<5>>("m")
        Assert.Equal("x2/m_0", m4.Name)
        ends x2
        let m5 = Mat<dim<200>, dim<5>>("m")
        Assert.Equal("m_2", m5.Name)

    [<Fact>]
    let ``Can export graph``() =
        let g = TensorGraph<dim<6>, dim<1>>()
        defaultGraph <- g
        let m0 = Mat<dim<100>, dim<60>>("m")
        let m1 = Mat<dim<100>, dim<60>>("m")
        Assert.Equal("m_0", m0.Name)
        Assert.Equal("m_1", m1.Name)
        let sum0 = 
            use x = scope "MatrixOps"
            m0 + m1
        let def = g._Graph.ExportAsGraphDef()
        Assert.NotEmpty(def.Node)
        do g._Graph.ExportToTxtFile("graph.pbtxt")
        ()
        //do g._Graph.ExportToGraphDef("graph.pbtxt")
        
        




        