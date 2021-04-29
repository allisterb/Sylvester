namespace Sylvester

open System

open FSharp.Quotations

open MathNet.Numerics

type Scalar<'t when 't: equality and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable> internal (e:Expr<'t>) =
    let expr = expand'<'t, 't> e
    member val Expr = expr
    member val Expr' = expr |> MathNetExpr.fromQuotation
    interface IScalar with
        member val Rank = Some 0 with get,set
        member val Dims = [| |] |> Some with get,set
      
    static member (+) (l:'t, r:Scalar<'t>) = 
        let e = call_add (Expr.Value(l)) (r.Expr) |> expand''<'t> in Scalar<'t> e
        