namespace Sylvester

open System

open FSharp.Quotations

open MathNet.Numerics

[<StructuredFormatDisplay("{Display}")>]
type Scalar<'t when 't: equality and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable> internal (e:Expr<'t>) =
    let expr = expand'<'t, 't> e
    let expr' = expr |> MathNetExpr.fromQuotation
    member val Expr = expr
    member val Expr' = expr'
    member val Val = evaluate expr
    interface IScalar with
        member val Rank = Some 0 with get,set
        member val Dims = [| |] |> Some with get,set
    member val Display = sprint' expr
    new(d:'t) = Scalar<@ d @>

    interface IComparable<Scalar<'t>> with
        member a.CompareTo b = a.Val.CompareTo b.Val
    
    interface IComparable with
        member a.CompareTo b = 
            match b with
            | :? Scalar<'t> as bs -> (a :> IComparable<Scalar<'t>>).CompareTo bs
            | :? 't as bs -> a.Val.CompareTo bs
            | _ -> failwith "This object is not a set."
    
    static member Zero = Unchecked.defaultof<'t> |> Scalar<'t>

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

[<RequireQualifiedAccess>]
module NumericLiteralR = 
  let FromZero() = Scalar<real> 0.
  let FromOne() = Scalar<real> 1.
  let FromInt32 (i:int) = i |> real |> Scalar<real>