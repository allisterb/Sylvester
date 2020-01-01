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
        resetDefaultGraph()
        let g = TensorGraph<dim<6>, dim<1>>("g")
        //checklt(g.Inputs.Length, nine)
        //check(g.Inputs.Length +> ten)
        let m0 = Mat<dim<100>, dim<50>>("m0")
        let m1 = Mat<dim<100>, dim<60>>("m1")
        let r = m0 + m1

        //let s = Scalar<float32
        //let p = m0 * s
        

        
        

        