#r ".\\..\src\\Base\\Sylvester.Arithmetic.FixedPoint\\bin\\Debug\\net45\\Sylvester.Arithmetic.FixedPoint.dll"
//#r ".\\..\src\\Providers\\Sylvester.Provider.Arithmetic.Internal\\src\\Sylvester.Provider.Arithmetic.Runtime\\bin\\Release\\net45\\Sylvester.Provider.Arithmetic.Runtime.dll"
#r ".\\..\src\\Base\\Sylvester.Arithmetic.Collections\\bin\\Debug\\net45\\Sylvester.Arithmetic.Collections.dll"
//#r ".\\..\src\\Base\\Sylvester.Tensors\\bin\\Debug\\net45\\FsAlg.NETStandard.dll"
#r ".\\..\src\\Base\\Sylvester.Tensors\\bin\\Debug\\net45\\Sylvester.Tensors.dll"



open Sylvester.Arithmetic.Collections
    
open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10
open Sylvester.Tensors

    
let varray(n:N10<'d10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>) = VArray<int, 'd10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>(n, Array.zeroCreate((int) n))


let x = varray(four)

x.SetVal(zero, 2)

//let y = vec four Arr


//let g = x.[one..two]

//g
//let h = v.[five] // Ok
    
//v.SetVal(five, 23) // Ok

//v._Array
//let z = v.At<N<90>>() //ok

//let g = v.[two * hundred] //type error
    
//v.SetVal(thousand, 0) //type error

//let y = v.at<N<101>>() //type error

//let t = Matrix<N<100>, N<24>, float>()

//let s = Matrix<N<24>, N<50>, float>()

//let a = t * s

//let G = FsAlg.Generic.Matrix(Array2D.create 4 5 1.)
//let H = FsAlg.Generic.Matrix(Array2D.create 4 5 1.)
//G + H

    //let  x = new Mat






                             
                    

    
    

       
