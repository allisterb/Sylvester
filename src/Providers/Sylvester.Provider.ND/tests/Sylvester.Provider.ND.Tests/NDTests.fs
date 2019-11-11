module Sylvester.Provider.ND.Tests
open Sylvester.Fabric.Keras
open Sylvester.Arithmetic

open Numpy

open NUnit.Framework

[<Test>]
let ``Default constructor should create instance`` () =
    let f = new ND<2, 1>(int32.GetDtype())
    let x = new ND<4,6>()
    
    ()
    

