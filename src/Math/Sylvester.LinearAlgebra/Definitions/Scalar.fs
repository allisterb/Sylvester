namespace Sylvester

open System

open FSharp.Quotations

open MathNet.Numerics

[<StructuredFormatDisplay("{Display}")>]
type Scalar<'t when 't: equality and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable> (e:Expr<'t>) =
    let expr = expand'<'t, 't> e
    let expr' = expr |> MathNetExpr.fromQuotation
    member val Expr = expr
    member val Expr' = expr'
    member val Val = evaluate expr
    member val Val' = box (evaluate expr)
    interface IScalar with
        member val Rank = Some 0 with get,set
        member val Dims = [| |] |> Some with get,set
    member val Display = sprint' expr
    new(d:'t) = Scalar<@ d @>

    interface IEquatable<Scalar<'t>> with
        member a.Equals b = a.Display = b.Display

    interface IComparable<Scalar<'t>> with
        member a.CompareTo b = if a.Val' :? IComparable<'t> then (a.Val' :?> IComparable<'t>).CompareTo b.Val else failwithf "The scalar type %A is not comparable" typeof<'t>
    
    interface IComparable with
        member a.CompareTo b = 
            match b with
            | :? Scalar<'t> as bs -> (a :> IComparable<Scalar<'t>>).CompareTo bs
            | :? 't as bs -> if a.Val' :? IComparable<'t> then (a.Val' :?> IComparable<'t>).CompareTo bs else failwithf "The scalar type %A is not comparable." typeof<'t>
            | _ -> failwith "This object is not a set."
    
    override a.Equals (_b:obj) = 
            match _b with 
            | :? Scalar<'t> as b -> (a :> IEquatable<Scalar<'t>>).Equals b
            | _ -> false
    
    static member Zero = typeof<'t> |> zero_val |> expand''<'t> |> Scalar

    static member One = typeof<'t> |> zero_val |> expand''<'t> |> Scalar

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

[<AutoOpen>]
module Scalar =
    let scalar<'t when 't: equality and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable> (n:'t) = Scalar n  