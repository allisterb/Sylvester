namespace Sylvester

open System

open FSharp.Quotations

[<StructuredFormatDisplay("{Display}")>]
type Scalar<'t when 't: equality and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't : equality  and 't :> IFormattable> (e:Expr<'t>) =
    let expr = expand_as<'t> e
    let expr' = expr |> MathNetExpr.fromQuotation
    member val Expr = expr
    member val Expr' = expr'
    member val Val = match expr with | Patterns.Value(v, t) -> v :?> 't |> Some | _ -> None
    interface IScalar with
        member val Rank = Some 0 with get,set
        member val Dims = [| |] |> Some with get,set
    member val Display = sprinte expr
    new(d:'t) = let e = expand_as<'t> <@ d @> in Scalar e

    interface IEquatable<Scalar<'t>> with
        member a.Equals b = a.Display = b.Display

    interface IComparable<Scalar<'t>> with
       member a.CompareTo b = a.Expr.ToString().CompareTo (b.Expr.ToString()) 
    
    interface IComparable with
        member a.CompareTo b = 
            match b with
            | :? Scalar<'t> as bs -> (a :> IComparable<Scalar<'t>>).CompareTo bs
            | _ -> failwith "This object is not a scalar value."
    
    override a.Equals (_b:obj) = 
            match _b with 
            | :? Scalar<'t> as b -> (a :> IEquatable<Scalar<'t>>).Equals b
            | _ -> false
    
    override a.GetHashCode() = a.Display.GetHashCode()

    static member Zero = typeof<'t> |> zero_val |> expand_as<'t> |> Scalar

    static member One = typeof<'t> |> zero_val |> expand_as<'t> |> Scalar

    static member op_Implicit (l:'t):Scalar<'t> = Scalar l

    static member (+) (l:Scalar<'t>, r:Scalar<'t>) = 
        let e = call_add (l.Expr) (r.Expr) |> expand_as<'t> in Scalar<'t> e
        
    static member (+) (l:'t, r:Scalar<'t>) = 
        let e = call_add (Expr.Value l) (r.Expr) |> expand_as<'t> in Scalar<'t> e

    static member (+) (l:Scalar<'t>, r:'t) = 
        let e = call_add (l.Expr) (Expr.Value r) |> expand_as<'t> in Scalar<'t> e

    static member (-) (l:Scalar<'t>, r:Scalar<'t>) = 
        let e = call_sub (l.Expr) (r.Expr) |> expand_as<'t> in Scalar<'t> e

    static member (-) (l:'t, r:Scalar<'t>) = 
        let e = call_sub (Expr.Value l) (r.Expr) |> expand_as<'t> in Scalar<'t> e
        
    static member (-) (l:Scalar<'t>, r:'t) = 
        let e = call_sub (l.Expr) (Expr.Value r) |> expand_as<'t> in Scalar<'t> e

    static member (/) (l:Scalar<'t>, r:Scalar<'t>) = 
        let e = call_div (l.Expr) (r.Expr) |> expand_as<'t> in Scalar<'t> e

    static member (/) (l:'t, r:Scalar<'t>) = 
        let e = call_div (Expr.Value l) (r.Expr) |> expand_as<'t> in Scalar<'t> e
        
    static member (/) (l:Scalar<'t>, r:'t) = 
        let e = call_div (l.Expr) (Expr.Value r) |> expand_as<'t> in Scalar<'t> e

    static member (*) (l:Scalar<'t>, r:Scalar<'t>) = 
        let e = call_mul (l.Expr) (r.Expr) |> expand_as<'t> in Scalar<'t> e

    static member (*) (l:'t, r:Scalar<'t>) = 
        let e = call_mul (Expr.Value l) (r.Expr) |> expand_as<'t> in Scalar<'t> e
        
    static member (*) (l:Scalar<'t>, r:'t) = 
        let e = call_mul (l.Expr) (Expr.Value r) |> expand_as<'t> in Scalar<'t> e

[<AutoOpen>]
module Scalar =
    let scalar (n:'t) = let e =  expand_as<'t> <@ n @> in Scalar(e)

    let sval (s:Scalar<'t>) = evaluate s.Expr



    let int_expr x = 
        match box x with
        | :? Scalar<int> as s -> s.Expr
        | :? Expr<int> as e -> e
        | :? int as n -> Expr.Value n |> expand_as<int>
        | :? real as n -> Expr.Value ((int) n) |> expand_as<int>
        | _ -> failwithf "The expression %A is not an integer expression." x

    let real_expr x = 
        match box x with
        | :? Scalar<real> as s -> s.Expr
        | :? Expr<real> as e -> e
        | :? real as n -> Expr.Value n |> expand_as<real>
        | :? int as n -> Expr.Value (real n) |> expand_as<real>
        | _ -> failwithf "The expression %A is not a real number expression." x

    let rat_expr x = 
        match box x with
        | :? Scalar<rat> as s -> s.Expr
        | :? Expr<rat> as e -> e
        | :? rat as n -> Expr.Value n |> expand_as<rat>
        | :? int as n -> Expr.Value (rat n) |> expand_as<rat>
        | _ -> failwithf "The expression %A is not a rational number expression." x