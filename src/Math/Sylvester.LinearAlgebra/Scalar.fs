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
      
    static member (+) (l:Scalar<'t>, r:Scalar<'t>) = 
        let e = call_add (l.Expr) (r.Expr) |> expand''<'t> in Scalar<'t> e

    static member (+) (l:'t, r:Scalar<'t>) = 
        let e = call_add (Expr.Value l) (r.Expr) |> expand''<'t> in Scalar<'t> e
        
    static member (+) (l:Scalar<'t>, r:'t) = 
        let e = call_add (l.Expr) (Expr.Value r) |> expand''<'t> in Scalar<'t> e

    static member (-) (l:Scalar<'t>, r:Scalar<'t>) = 
        let e = call_sub (l.Expr) (r.Expr) |> expand''<'t> in Scalar<'t> e

    static member (-) (l:'t, r:Scalar<'t>) = 
        let e = call_sub (Expr.Value l) (r.Expr) |> expand''<'t> in Scalar<'t> e
        
    static member (-) (l:Scalar<'t>, r:'t) = 
        let e = call_sub (l.Expr) (Expr.Value r) |> expand''<'t> in Scalar<'t> e

    static member (/) (l:Scalar<'t>, r:Scalar<'t>) = 
        let e = call_div (l.Expr) (r.Expr) |> expand''<'t> in Scalar<'t> e

    static member (/) (l:'t, r:Scalar<'t>) = 
        let e = call_div (Expr.Value l) (r.Expr) |> expand''<'t> in Scalar<'t> e
        
    static member (/) (l:Scalar<'t>, r:'t) = 
        let e = call_div (l.Expr) (Expr.Value r) |> expand''<'t> in Scalar<'t> e

    static member (*) (l:Scalar<'t>, r:Scalar<'t>) = 
        let e = call_mul (l.Expr) (r.Expr) |> expand''<'t> in Scalar<'t> e

    static member (*) (l:'t, r:Scalar<'t>) = 
        let e = call_mul (Expr.Value l) (r.Expr) |> expand''<'t> in Scalar<'t> e
        
    static member (*) (l:Scalar<'t>, r:'t) = 
        let e = call_mul (l.Expr) (Expr.Value r) |> expand''<'t> in Scalar<'t> e

[<AutoOpen>]
module Scalar =
    let sexpr (s:Scalar<_>) = s.Expr

    let ssimplify (s:Scalar<_>) = s.Expr |> simplify |> Scalar<_>