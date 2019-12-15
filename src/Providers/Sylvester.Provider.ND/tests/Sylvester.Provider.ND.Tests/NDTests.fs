module Sylvester.Provider.ND.Tests

open Sylvester.Fabric.Keras
open Sylvester.Arithmetic

open Numpy

let ``Default constructor should create instance`` () =
    let f = new ND<2, 1>(int32.GetDtype())
    let h = new ND<2, 1>(int32.GetDtype())
    let g = f + h
    //let r = ND<2,1>.Test()
    //let x = new ND<4,6>()
    //let x =ND<0>.
    //let y = x
    ()
    

