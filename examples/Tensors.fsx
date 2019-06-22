#r ".\\..\src\\Providers\\Sylvester.Provider.Tensors\\src\\Sylvester.Provider.Tensors.Runtime\\bin\\Release\\net45\\Mathnet.Numerics.dll"
#r ".\\..\src\\Providers\\Sylvester.Provider.Tensors\\src\\Sylvester.Provider.Tensors.Runtime\\bin\\Release\\net45\\Mathnet.Numerics.FSharp.dll"
#r ".\\..\src\\Providers\\Sylvester.Provider.Arithmetic\\src\\Sylvester.Provider.Arithmetic.Runtime\\bin\\Release\\net45\\Sylvester.Provider.Arithmetic.Runtime.dll"
#r ".\\..\src\\Providers\\Sylvester.Provider.Tensors\\src\\Sylvester.Provider.Tensors.Runtime\\bin\\Release\\net45\\Sylvester.Provider.Tensors.Runtime.dll"


open Sylvester.Arithmetic.Collections
    
open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10
open Sylvester.Tensors

    
let varray(n:N10<'d10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>) = VArray<int, 'd10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>(n, Array.zeroCreate((int) n))

let xxxx = new N<100>()

let x = varray(xxxx)

x.SetVal(zero, 2)

x.[N<90>.i]

//let  h = Vector<float, _0, _0>()
let j = new Vec<700>()

let f = j.[hundred]

let z = vmax j

let r = Vec<700>.Rand

let s = Vec<900>.Rand

let vvv = Vec<4>.Rand

let m44 = new Mat<4, 4>()

let jjj = m44 +@. vvv



//let u = app

//let m = new Mat<800, 700>()

//let h = new Mat<4, 4>()


//f = j.[thousand]


//.

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






                             
                    

    
    

       
