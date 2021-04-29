namespace Sylvester

open System
open FSharp.Quotations

open MathNet.Numerics.LinearAlgebra

type Expr' = MathNet.Symbolics.Expression

type _Vector<'t when 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable> = Vector<'t>

type _Matrix<'t when 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable> = Matrix<'t>

(*
type IScalarSymbolicOps =
    abstract Add:Expr<'t> -> Expr<'t> -> Expr<'t>
    abstract AddL:Expr<'t> -> 't -> Expr<'t>
    abstract AddR:'t -> Expr<'t> -> Expr<'t>
    abstract Subtract:Expr<'t> -> Expr<'t> -> Expr<'t>
    abstract Multiply:Expr<'t> -> Expr<'t> -> Expr<'t>
    abstract Divide:Expr<'t> -> Expr<'t> -> Expr<'t>
*)
type IVectorNumericOps =
    abstract Add:_Vector<'t> -> _Vector<'t> -> _Vector<'t>
    abstract Subtract:_Vector<'t> -> _Vector<'t> -> _Vector<'t>
    abstract InnerProduct:_Vector<'t> -> _Vector<'t> -> 't

type IVectorSymbolicOps =
    abstract Add:Expr<'t> array -> Expr<'t> array -> Expr<'t> array
    abstract Subtract:Expr<'t> array -> Expr<'t> array -> Expr<'t> array
    abstract InnerProduct:Expr<'t> array -> Expr<'t> array -> Expr<'t>
    
type IMatrixNumericOps = 
    abstract MatAdd:_Matrix<'t> -> _Matrix<'t> -> _Matrix<'t>
    abstract MatSubtract:_Matrix<'t> -> _Matrix<'t> -> _Matrix<'t>
    abstract MatMultiply:_Matrix<'t> -> _Matrix<'t> -> _Matrix<'t>

type ILinearAlgebraNumericOps =
    inherit IVectorNumericOps
    inherit IMatrixNumericOps

type ILinearAlgebraSymbolicOps =
    inherit IVectorSymbolicOps
    
module private Ops =
    let vars (a:Expr<_> array list) = a |> List.map Array.toList |> List.concat |> List.map get_vars |> List.concat

    let exprs (a:Expr<_> array list) = a |> List.map Array.toList |> List.concat |> List.map MathNetExpr.fromQuotation
    
    let fromQ e = Array.map MathNetExpr.fromQuotation e

    let toQ<'t> v e = MathNetExpr.toQuotation'<'t> v e

type MathNetLinearAlgebraNumeric() =
    interface ILinearAlgebraNumericOps with
        member x.Add l r =  l.Add r
        member x.Subtract l r = l.Subtract r
        member x.InnerProduct l r = l.DotProduct r

        member x.MatAdd l r = l.Add r
        member x.MatSubtract l r = l.Subtract r
        member x.MatMultiply l r = l.Multiply r

type MathNetLinearAlgebraSymbolic() =
    interface ILinearAlgebraSymbolicOps with
        member x.Add l r = Array.map2 call_add l r |> Array.map expand''
        
        member x.Subtract l r = 
            let vars = Ops.vars [l;r]
            let l',r' = Ops.fromQ l, Ops.fromQ r             
            Array.map2 (-) l' r' 
            |> Array.map (Ops.toQ<'t> vars)
            |> Array.map (fun e -> <@ %%e:'t @>)

        member x.InnerProduct l r = 
            let r = 
                let vars = Ops.vars [l;r]
                let l', r' = Ops.fromQ l, Ops.fromQ r             
                Array.zip l' r' 
                |> Array.map(fun(a, b) -> a * b)
                |> Array.reduce (+)
                |> Ops.toQ<'t> vars
            <@ %%r:'t @>

[<AutoOpen>]
module LinearAlgbra =
    let (|LinearAlgebraNumericOpType|_|):Type->Type option =
        function
        | t when t.Name = "Single" || t.Name = "Double" || t.Name = "Complex" || t.Name = "Complex32" -> Some t
        | _ -> None
    
    let mutable defaultLinearAlgebraNumericOps = new MathNetLinearAlgebraNumeric() :> ILinearAlgebraNumericOps

    let mutable defaultLinearAlgebraSymbolicOps = new MathNetLinearAlgebraSymbolic() :> IVectorSymbolicOps