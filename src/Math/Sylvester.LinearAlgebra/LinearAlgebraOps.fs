namespace Sylvester

open System
open FSharp.Quotations


open MathNet.Numerics.LinearAlgebra

type _Vector<'t when 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable> = Vector<'t>

type _Matrix<'t when 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable> = Matrix<'t>

type IVectorOps =
    abstract VecAdd:_Vector<'t> -> _Vector<'t> -> _Vector<'t>
    abstract VecSubtract:_Vector<'t> -> _Vector<'t> -> _Vector<'t>
    abstract VecDotProduct:_Vector<'t> -> _Vector<'t> -> 't

type IMatrixOps = 
    abstract MatAdd:_Matrix<'t> -> _Matrix<'t> -> _Matrix<'t>
    abstract MatSubtract:_Matrix<'t> -> _Matrix<'t> -> _Matrix<'t>
    abstract MatMultiply:_Matrix<'t> -> _Matrix<'t> -> _Matrix<'t>

type ILinearAlgebraOps =
    inherit IVectorOps
    inherit IMatrixOps

type MathNetLinearAlgebra() =
    interface ILinearAlgebraOps with
        member x.VecAdd l r =  l.Add r
        member x.VecSubtract l r = l.Subtract r
        member x.VecDotProduct l r = l.DotProduct r

        member x.MatAdd l r = l.Add r
        member x.MatSubtract l r = l.Subtract r
        member x.MatMultiply l r = l.Multiply r

[<AutoOpen>]
module LinearAlgbra =
    let (|MathNetLinearAlgebraSupportedType|_|):Type->Type option =
        function
        | t when t.Name = "Single" || t.Name = "Double" || t.Name = "Complex" || t.Name = "Complex32" -> Some t
        | _ -> None
    
    let (|WithNoVariables|_|):Expr->Type option =
        function
        | e when (get_vars e) = List.empty -> Some e.Type
        | _ -> None
    
    let mutable defaultLinearAlgebraOps = new MathNetLinearAlgebra() :> ILinearAlgebraOps

