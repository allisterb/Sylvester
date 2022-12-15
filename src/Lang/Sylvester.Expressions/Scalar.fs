namespace Sylvester

open System

open FSharp.Quotations

[<StructuredFormatDisplay("{Display}")>]
type Scalar6<'t when 't: equality and 't: comparison and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IFormattable> (e:Expr<'t>) =
    let expr = expand''<'t> e
    let mnexpr = MathNetExpr.fromQuotation expr
    member val Expr = expr
    member val MathNetExpr = mnexpr
    member val Val = match expr with | Patterns.Value(v, _) -> v :?> 't |> Some | _ -> None
    member val Display = sprinte expr
    new(d:'t) = let e = expand''<'t> <@ d @> in Scalar6 e
    interface IEquatable<Scalar6<'t>> with
        member a.Equals b = a.Display = b.Display

    interface IComparable<Scalar6<'t>> with
       member a.CompareTo b = a.Expr.ToString().CompareTo (b.Expr.ToString()) 
    
    interface IComparable with
        member a.CompareTo b = 
            match b with
            | :? Scalar6<'t> as bs -> (a :> IComparable<Scalar6<'t>>).CompareTo bs
            | _ -> failwith "This object is not a scalar value."
    
    override a.Equals (_b:obj) = 
            match _b with 
            | :? Scalar6<'t> as b -> (a :> IEquatable<Scalar6<'t>>).Equals b
            | _ -> false
    
    override a.GetHashCode() = a.Display.GetHashCode()

    static member Zero = typeof<'t> |> zero_val |> expand''<'t> |> Scalar6

    static member One = typeof<'t> |> zero_val |> expand''<'t> |> Scalar6

    static member op_Implicit (l:'t):Scalar6<'t> = Scalar6 l

    static member op_Implicit (l:int):Scalar6<real> = let v = (real) l in Scalar6 v

    static member op_Implicit (l:rat):Scalar6<real> = let v = (real) l in Scalar6 v

    static member (+) (l:Scalar6<'t>, r:Scalar6<'t>) = call_add (l.Expr) (r.Expr) |> expand''<'t> |> Scalar6<'t>
    
    static member (+) (l:Scalar6<'t>, r:'t) = call_add (l.Expr) (Expr.Value r) |> expand''<'t> |> Scalar6<'t>

    static member (+) (l:'t, r:Scalar6<'t>) = call_add (Expr.Value l) r.Expr |> expand''<'t> |> Scalar6<'t>

    static member (+) (l:Scalar6<real>, r:int) = let r' = (real) r in call_add (l.Expr) (Expr.Value r') |> expand''<'t> |> Scalar6<'t>

    static member (+) (l:Scalar6<real>, r:rat) = let r' = (real) r in call_add (l.Expr) (Expr.Value r') |> expand''<'t> |> Scalar6<'t>
    
    static member (+) (l:Scalar6<real>, r:nat) = let r' = (real) r in call_add (l.Expr) (Expr.Value r') |> expand''<'t> |> Scalar6<'t>

    static member (+) (l:int, r:Scalar6<real>) = let l' = (real) l in call_add (Expr.Value l') r.Expr |> expand''<'t> |> Scalar6<'t>

    static member (+) (l:rat, r:Scalar6<real>) = let l' = (real) l in call_add (Expr.Value l') r.Expr |> expand''<'t> |> Scalar6<'t>
    
    static member (+) (l:nat, r:Scalar6<real>) = let l' = (real) l in call_add (Expr.Value l') r.Expr |> expand''<'t> |> Scalar6<'t>
    
    static member (+) (l:Scalar6<rat>, r:int) = call_add (l.Expr) (Expr.Value (rat r)) |> expand''<'t> |> Scalar6<'t>

    static member (+) (l:int, r:Scalar6<rat>) = call_add (Expr.Value (rat l)) r.Expr |> expand''<'t> |> Scalar6<'t>

    static member (-) (l:Scalar6<'t>, r:Scalar6<'t>) = call_sub (l.Expr) (r.Expr) |> expand''<'t> |> Scalar6<'t> 

    static member (-) (l:Scalar6<'t>, r:'t) = call_sub (l.Expr) (Expr.Value r) |> expand''<'t> |> Scalar6<'t>

    static member (-) (l:'t, r:Scalar6<'t>) = call_sub (Expr.Value l) r.Expr |> expand''<'t> |> Scalar6<'t>

    static member (/) (l:Scalar6<'t>, r:Scalar6<'t>) = call_div (l.Expr) (r.Expr) |> expand''<'t> |> Scalar6<'t>

    static member (/) (l:Scalar6<'t>, r:'t) = call_div (l.Expr) (Expr.Value r) |> expand''<'t> |> Scalar6<'t>

    static member (/) (l:'t, r:Scalar6<'t>) = call_div (Expr.Value l) r.Expr |> expand''<'t> |> Scalar6<'t>

    static member (*) (l:Scalar6<'t>, r:Scalar6<'t>) = call_mul (l.Expr) (r.Expr) |> expand''<'t> |> Scalar6<'t>
    
    static member (*) (l:Scalar6<'t>, r:'t) = call_mul (l.Expr) (Expr.Value r) |> expand''<'t> |> Scalar6<'t>

    static member (*) (l:'t, r:Scalar6<'t>) = call_mul (Expr.Value l) r.Expr |> expand''<'t> |> Scalar6<'t>

[<RequireQualifiedAccess>]
module NumericLiteralS = 
  let FromZero() = Scalar6 0
  let FromOne() = Scalar6 1
  

module Scalar6 =
    let int_expr x = 
        match box x with
        | :? Scalar6<int> as s -> s.Expr
        | :? Expr<int> as e -> e
        | :? int as n -> Expr.Value n |> expand''<int>
        | :? real as n -> Expr.Value ((int) n) |> expand''<int>
        | _ -> failwithf "The expression %A is not an integer expression." x

    let real_expr x = 
        match box x with
        | :? Scalar6<real> as s -> s.Expr
        | :? Expr<real> as e -> e
        | :? real as n -> Expr.Value n |> expand''<real>
        | :? int as n -> Expr.Value (real n) |> expand''<real>
        | _ -> failwithf "The expression %A is not a real number expression." x

    let rat_expr x = 
        match box x with
        | :? Scalar6<rat> as s -> s.Expr
        | :? Expr<rat> as e -> e
        | :? rat as n -> Expr.Value n |> expand''<rat>
        | :? int as n -> Expr.Value (rat n) |> expand''<rat>
        | _ -> failwithf "The expression %A is not a rational number expression." x

    let var6<'t when 't: equality and 't: comparison and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IFormattable> name = 
        Expr.Var(Var(name, typeof<'t>)) |> expand''<'t> |> Scalar6<'t>