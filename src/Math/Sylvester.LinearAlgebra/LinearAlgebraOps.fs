namespace Sylvester

open System

open MathNet.Numerics.LinearAlgebra

type MNVector<'t when 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable> = Vector<'t>

type MNMatrix<'t when 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable> = Matrix<'t>

type IVectorOps =
    abstract VecAdd:Vector<'t> -> Vector<'t> -> Vector<'t>
    abstract VecSubtract:Vector<'t> -> Vector<'t> -> Vector<'t>
    abstract VecDotProduct:Vector<'t> -> Vector<'t> -> 't

type IMatrixOps = 
    abstract MatAdd:Matrix<'t> -> Matrix<'t> -> Matrix<'t>
    abstract MatSubtract:Matrix<'t> -> Matrix<'t> -> Matrix<'t>
    abstract MatMultiply:Matrix<'t> -> Matrix<'t> -> Matrix<'t>

type ILinearAlgebraOps =
    inherit IVectorOps
    inherit IMatrixOps
    
type MathNetLinearAlgebra() =
    interface ILinearAlgebraOps with
        member x.VecAdd l r = l.Add r
        member x.VecSubtract l r = l.Subtract r
        member x.VecDotProduct l r = l.DotProduct r

        member x.MatAdd l r = l.Add r
        member x.MatSubtract l r = l.Subtract r
        member x.MatMultiply l r = l.Multiply r

[<AutoOpen>]
module LinearAlgbra =
    let mutable defaultLinearAlgebraOps = new MathNetLinearAlgebra() :> ILinearAlgebraOps