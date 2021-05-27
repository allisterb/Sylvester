namespace Sylvester

open System

open FSharp.Quotations

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
    new(d:'t) = let e = expand''<'t> <@ d @> in Scalar e

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
    
    override a.GetHashCode() = a.Display.GetHashCode()

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
    let scalar (n:'t) = let e =  expand''<'t> <@ n @> in Scalar(e)

    let sval (s:Scalar<'t>) = s.Val

    let realvar n = var'<real> n
    
    let intvar n = var'<int> n

    let int_expr x = 
        match box x with
        | :? Scalar<int> as s -> s.Expr
        | :? Expr<int> as e -> e
        | :? int as n -> Expr.Value n |> expand''<int>
        | :? real as n -> Expr.Value ((int) n) |> expand''<int>
        | _ -> failwithf "The expression %A is not an integer expression." x

    let real_expr x = 
        match box x with
        | :? Scalar<real> as s -> s.Expr
        | :? Expr<real> as e -> e
        | :? real as n -> Expr.Value n |> expand''<real>
        | :? int as n -> Expr.Value (real n) |> expand''<real>
        | _ -> failwithf "The expression %A is not a real number expression." x
