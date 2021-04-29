namespace Sylvester

open System

open FSharp.Quotations

open MathNet.Numerics

type Scalar<'t when 't: equality and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable> internal (e:Expr<'t>, value:'t, isSymbolic:bool) =
    let expr = expand'<'t, 't> e
    let data = value
    member val Expr = expr
    member val Expr' = expr |> MathNetExpr.fromQuotation
    member val Value = data
    member val _V = LinearAlgebra.DenseVector.raw [|data|]  
    member val IsSymbolic = isSymbolic
    interface IScalar with
        member val Rank = Some 0 with get,set
        member val Dims = [| |] |> Some with get,set
        member val Data = [|data|] :> System.Array with get,set
    new (v:'t) = Scalar<'t>(<@ v @>, v, false)
    new (e:Expr<'t>) = Scalar(e, evaluate e, true)

    static member (+) (l:'t, r:Scalar<'t>) = 
        let e = call_add (Expr.Value(l)) (r.Expr) |> expand''<'t> in Scalar<'t>(e, (evaluate e), true)
        