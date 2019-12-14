namespace Sylvester.Fabric.Keras



module Say =
    open Numpy
    open Sylvester.Fabric.Keras
    open Sylvester.Arithmetic
    type ND with
        member x.foo = "gg"
    
    let hello name =
        let v = new ND<2,4>()
        let r = v.foo
        printfn "Hello %s" name
