namespace Sylvester.Fabric.Keras


module Say =
    open Numpy
    open Sylvester.Fabric.Keras
    open Sylvester.Arithmetic
    let hello name =
        let v = new ND<2,4>()
        v
        printfn "Hello %s" name
