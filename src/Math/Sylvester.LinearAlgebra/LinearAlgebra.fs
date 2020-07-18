namespace Sylvester

open MathNet.Numerics

type ILinearAlgebraOps = 
    abstract Multiply:Matrix<'m, 'n, 't> -> Matrix<'n ,'p, 't> -> Matrix<'n, 'p, 't>


//type MathNateLinearAlgebra() =
//    interface ILinearAlgebraOps with
        //member Multiply(l, r) = LinearAlgebra.Matrix.
