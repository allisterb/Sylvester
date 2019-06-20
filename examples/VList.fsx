#r ".\\..\src\\Base\\Sylvester.FixedPoint\\bin\\Debug\\net45\\Sylvester.FixedPoint.dll"
#r ".\\..\src\\Providers\\Sylvester.Provider.Arithmetic.Internal\\src\\Sylvester.Provider.Arithmetic.Runtime\\bin\\Release\\net45\\Sylvester.Provider.Arithmetic.Runtime.dll"
#r ".\\..\src\\Base\\Sylvester.Arithmetic.Collections\\bin\\Debug\\net45\\Sylvester.Arithmetic.Collections.dll"
#r ".\\..\src\\Base\\Sylvester.Tensors\\bin\\Debug\\net45\\FsAlg.NETStandard.dll"
#r ".\\..\src\\Base\\Sylvester.Tensors\\bin\\Debug\\net45\\Sylvester.Tensors.dll"



open Sylvester.Arithmetic.Collections
    
open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10
open Sylvester.Tensors

    
let v = VArray<N<100>, int>()
let h = v.[five] // Ok
    
v.SetVal(five, 23) // Ok
    
let z = v.At<N<90>>() //ok

//let g = v.[two * hundred] //type error
    
//v.SetVal(thousand, 0) //type error

//let y = v.at<N<101>>() //type error

let t = Matrix<N<100>, N<24>, float>()

let s = Matrix<N<24>, N<50>, float>()

let a = t * s

    //let  x = new Mat






                             
                    

    
    

       
