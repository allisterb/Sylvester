#r ".\\..\\..\\src\\Providers\\Sylvester.Provider.Tensors\\src\\Sylvester.Provider.Tensors.Runtime\\bin\\Release\\net45\\Mathnet.Numerics.dll"
#r ".\\..\\..\\src\\Providers\\Sylvester.Provider.Tensors\\src\\Sylvester.Provider.Tensors.Runtime\\bin\\Release\\net45\\Mathnet.Numerics.FSharp.dll"
#r ".\\..\\..\\src\\Providers\\Sylvester.Provider.Arithmetic\\src\\Sylvester.Provider.Arithmetic.Runtime\\bin\\Release\\netstandard2.0\\Sylvester.Provider.Arithmetic.Runtime.dll"
#r ".\\..\\..\\src\\Providers\\Sylvester.Provider.Tensors\\src\\Sylvester.Provider.Tensors.Runtime\\bin\\Release\\netstandard2.0\\Sylvester.Provider.Tensors.Runtime.dll"
//#r ".\\..\\..\\src\\Providers\\Sylvester.Provider.NDArray\\src\\Sylvester.Provider.NDArray.Runtime\\bin\\Release\\netstandard2.0\\Sylvester.Provider.NDArray.Runtime.dll"


open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10
open Sylvester.Tensors

// Vectors
let v100 = Vec<100>.One
let v200= Vec<200>.Rand

// !+ is length operator

let l = !+ v200
let v200b = Vec<200>.Rand

let s1 = v200 + v200b

//let s2 = v200 + v100 //Type error

let p1 = v200 * v200b

let pv1 = v200 * scalar 4.0f

// let p2 = v200b * v100 //type error

let c1 = v100 +!> v200

let c2 = v100 +!< v200

//Load a vector of data from file
let vf = new VecF<"C:\\Projects\\Sylvester\\examples\\data\\iris.data", false, 1>()

let l2 = !+vf

let d = vf.Dim0

let inline mylinearfunc a b =
    check(!+a +> !+b)

mylinearfunc v200 v100
// Matrices

let m4200 = Mat<4, 200>.Rand

let m44 = Mat<4, 4>.Rand

let mnotsquare = Mat<1000, 700>.Rand

let vreallybig = Vec<100000>.Rand

let m44det = mdet m44

//let m4200det = mdet m4200 //Type error

let m44inv = minv m44

//let minv44 = minv m4200

let m920 = Mat<9, 20>.One

let m2018 = Mat<20, 18>.One

let r = m920 * m2018

let d1 = !+ r

let d2 = !++ r

//let rr = m2018 * m920

//let x = mylinearfunc v200 v100

let m45 = Mat<4, 5>.Rand
let v5c = Vec<5>.Rand
let r3 = m45 * v5c

let m99 = Mat<9, 9>.One

//let rr = m99 * v5c

let m99b = Mat<9, 9>.Rand

let s3 = m99.[zero..three, zero..four]

let sss = !+ s3

let m99one = Mat<9,9>.One

let s = m99one +@ (zero, Vec<9>.Rand)

let sd0 = !+ s
let sd1 = !++s