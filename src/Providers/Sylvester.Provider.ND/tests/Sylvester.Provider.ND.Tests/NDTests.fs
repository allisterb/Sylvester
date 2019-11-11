module Sylvester.Provider.ND.Tests
open Sylvester.Fabric.Keras

open Numpy

open NUnit.Framework

[<Test>]
let ``Default constructor should create instance`` () =
    let f = new ND<2, 1>()
    ()
    

