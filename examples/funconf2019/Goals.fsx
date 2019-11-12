#r ".\\..\\..\\src\\Providers\\Sylvester.Provider.Tensors\\src\\Sylvester.Provider.Tensors.Runtime\\bin\\Release\\net45\\Mathnet.Numerics.dll"
#r ".\\..\\..\\src\\Providers\\Sylvester.Provider.Tensors\\src\\Sylvester.Provider.Tensors.Runtime\\bin\\Release\\net45\\Mathnet.Numerics.FSharp.dll"
#r ".\\..\\..\\src\\Providers\\Sylvester.Provider.Arithmetic\\src\\Sylvester.Provider.Arithmetic.Runtime\\bin\\Release\\netstandard2.0\\Sylvester.Provider.Arithmetic.Runtime.dll"
#r ".\\..\\..\\src\\Providers\\Sylvester.Provider.Tensors\\src\\Sylvester.Provider.Tensors.Runtime\\bin\\Release\\netstandard2.0\\Sylvester.Provider.Tensors.Runtime.dll"
//#r ".\\..\\..\\src\\Providers\\Sylvester.Provider.NDArray\\src\\Sylvester.Provider.NDArray.Runtime\\bin\\Release\\netstandard2.0\\Sylvester.Provider.NDArray.Runtime.dll"


open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10
open Sylvester.Tensors

let v100 = vnew hundred 5.0f 

let v9000 = vrand (nine * thousand)

let v200= Vec<200>.Rand

let v200b = Vec<200>.Rand

let s1 = v200 + v200b

//let s2 = v200 + v100 //Type error

let p1 = v200 * v200b

let pv1 = v200 * scalar 4.0f

//let p2 = v200b * v100 //type error

let c1 = v100 +!> v200

let c2 = v100 +!< v200

let c3 =  (v200b +!< v200) <?> (44, "foo") 

let m4200 = Mat<4, 200>.Rand

let m44 = Mat<4, 4>.Rand

let mnotsquare = Mat<1000, 700>.Rand

let inline mylinearfunc a b =
    check(a +!> b)

let c4 = mylinearfunc v9000 v100

let vreallybig = Vec<100000>.Rand


//let c4 = foo v100 v9000 //Doesn't satisfy constraint


let m44det = mdet m44

let m44inv = minv m44

//let mnsinv = minv mnotsquare //Doesn't satisfy matrix function constraint


