namespace Sylvester

open System
open System.Collections
open System.Collections.Generic

open FSharp.Quotations
open MathNet.Numerics

open Arithmetic
open Dimension

[<StructuredFormatDisplay("{Display}")>]
type Vector<'t when 't: equality and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable>
    internal(e: Expr<'t> array) = 
    do if e.Length = 0 then failwith "The length of a vector must one or greater."
    let expr = e  |> Array.map expand'<'t, 't>
    let exprmn = Array.map MathNetExpr.fromQuotation expr
    
    member val Expr = expr
    member val ExprList = expr |> Array.toList
    member val ExprVars = expr |> Array.map (get_vars >> List.toArray) |> Array.concat
    member val ExprMathNet = exprmn
    
    member val Display = 
        expr 
        |> Array.skip 1 
        |> Array.fold(fun s e -> sprintf "%s, %s" s (sprint' e)) (sprint' expr.[0]) 
        |> sprintf "(%s)"
    
    member val LinearDisplay =
        expr 
        |> Array.skip 1 
        |> Array.fold(fun s e -> sprintf "%s %s" s (sprint' e)) (sprint' expr.[0]) 
        |> sprintf "%s"
    
    member x.AsNumeric() = 
        let t = typeof<'t>
        match t with
        | LinearAlgebraNumericOpType -> expr |> Array.map evaluate |> LinearAlgebra.DenseVector.raw
        | _ -> failwithf "The type %A is not compatible with numeric linear algebra operations." t
    
    member x.Item with get(i)  = e.[i] |> Scalar<'t>
    
    interface IPartialShape<``1``> with
        member val Rank = Some 1 with get,set
        member val Dims = [| Convert.ToInt64(e.Length) |] |> Some with get,set
    
    interface IEnumerable<Expr<'t>> with
        member x.GetEnumerator ()  = (x.Expr |> Array.toSeq).GetEnumerator()
        member x.GetEnumerator () = (x :> IEnumerable<Expr<'t>>).GetEnumerator () :> IEnumerator
    
    interface IEquatable<Vector<'t>> with
          member a.Equals b = a.LinearDisplay = b.LinearDisplay

    new([<ParamArray>] v:'t array) = let expr = v |> Array.map(fun e -> <@ e @>) in Vector<'t>(expr)
    
    new([<ParamArray>] v:Scalar<'t> array) = let expr = v |> Array.map(sexpr >> expand''<'t>) in Vector<'t>(expr)    
    
    new(v: Expr<'t list>) = let expr = v |> expand_list' |> List.toArray in Vector<'t>(expr)
    
    new(d:'t list) = Vector<'t>(List.toArray d)
    
    static member (+) (l: Vector<'t>, r: Vector<'t>) = 
        let e = defaultLinearAlgebraSymbolicOps.Add l.Expr r.Expr in Vector<'t>(e)

    static member create([<ParamArray>] data: 't array) = Vector<'t>(data)

[<StructuredFormatDisplay("{Display}")>]
type Vector<'dim0, 't when 'dim0 :> Number and 't: equality and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable>
    internal (e: Expr<'t> array) =
    inherit Vector<'t>(e)
    let dim0 = number<'dim0>
    do if e.Length <> dim0.IntVal then failwithf "The initializing array has length %i instead of %i." e.Length dim0.IntVal
    
    member val Dim0:'dim0 = dim0
    member val Display = base.Display
    member x.Norm = let p = x * x in p |> simplify |> call_sqrt |> expand''<'t> |> Scalar
    
    new([<ParamArray>] v:'t array) = let expr = v |> Array.map(fun e -> <@ e @>) in Vector<'dim0, 't>(expr)
    
    new([<ParamArray>] v:Scalar<'t> array) = let expr = v |> Array.map(sexpr >> expand''<'t>) in Vector<'dim0, 't>(expr)
    
    new(v: Expr<'t list>) = let expr = v |> expand_list' |> List.toArray in Vector<'dim0, 't>(expr)
    
    new(d:'t list) = Vector<'dim0, 't>(List.toArray d)
    
    interface IVector<'dim0> with member val Dim0 = dim0

    interface IEquatable<Vector<'dim0, 't>> with
        member a.Equals b = a.LinearDisplay = b.LinearDisplay
     
    static member Zero:Vector<'dim0, 't> = let e = Array.create number<'dim0>.IntVal (zero_val(typeof<'t>) |> expand''<'t>) in Vector<'dim0, 't> e

    static member One:Vector<'dim0, 't> = let e = Array.create number<'dim0>.IntVal (one_val(typeof<'t>) |> expand''<'t>) in Vector<'dim0, 't> e

    static member (+) (l: Vector<'dim0, 't>, r: Vector<'dim0, 't>) = 
        let e = defaultLinearAlgebraSymbolicOps.Add l.Expr r.Expr in Vector<'dim0, 't>(e)
    
    static member (-) (l: Vector<'dim0, 't>, r: Vector<'dim0, 't>) = 
        let e = defaultLinearAlgebraSymbolicOps.Subtract l.Expr r.Expr in Vector<'dim0, 't>(e)

    static member (*) (l: Vector<'dim0, 't>, r: Vector<'dim0, 't>) = 
        let e = defaultLinearAlgebraSymbolicOps.InnerProduct l.Expr r.Expr in Scalar<'t> e

    static member (*) (l: Scalar<'t>, r: Vector<'dim0, 't>) = 
        r.Expr |> Array.map(fun e -> expand''<'t> <| call_mul (l.Expr) e) |> Vector<'n, 't>

    static member (*) (l: Vector<'dim0, 't>, r: Scalar<'t>) = 
        l.Expr |> Array.map(fun e -> expand''<'t> <| call_mul e (r.Expr) ) |> Vector<'n, 't>

    static member (*) (l: Vector<'dim0, 't>, r: 't) : Vector<'dim0, 't> = let r' = Scalar<'t>(r) in l * r' 

    static member (*) (l: 't, r: Vector<'dim0, 't>) : Vector<'dim0, 't> = let l' = Scalar<'t>(l) in l' * r

    static member (~-) (l: Vector<'dim0, 't>) =
        l.Expr |> Array.map(call_neg >> expand''<'t>) |> Vector<'n, 't>

type Vec<'dim0 when 'dim0 :> Number> = Vector<'dim0, real>
type VecC<'dim0 when 'dim0 :> Number> = Vector<'dim0, complex>
type VecQ<'dim0 when 'dim0 :> Number> = Vector<'dim0, rat>
type VecZ<'dim0 when 'dim0 :> Number> = Vector<'dim0, int>

module Vector =
    let (|Vector|_|) (v: Vector<'n, 't>) : Expr<'t> list option = Some(v.ExprList)
    
    let vec (dim:'n) (data:Expr<real list>) = Vector<'n, real> data
    
    let vecz (dim:'n) (data:Expr<int list>) = Vector<'n, int> data
    
    let vecq  (dim:'n) (data:Expr<rat list>) = Vector<'n, rat> data

    let vecc (dim:'n) (data:Expr<complex list>) = Vector<'n, complex> data

    let vvars<'n, 't when 'n :> Number and 't: equality and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable> s = vars<'t> s (number<'n>.IntVal) |> Vector<'n, 't> 
    
    let vexpr(l:Vector<'n, 't>) = l.Expr

    let add (l:Vector<'n, 't>) (r:Vector<'n, 't>) = l + r
    
    let sub (l:Vector<'n, 't>) (r:Vector<'n, 't>) = l - r
    
    let smul (l:'t) (r:Vector<'n, 't>) = Vector<'n, 't>.(*) (l, r)

    let inner_product_val (l:Vector<'n,'t>) (r:Vector<'n,'t>) = (l * r) |> sval
    
    let norm (l:Vector<'n, 't>) =
        let p = l * l in p |> simplify |> call_sqrt |> expand''<'t>  |> Scalar<'t> 

    let euclid_dist (l:Vector<'n, 't>) (r:Vector<'n, 't>) = (l - r) |> norm |> simplify |> Scalar<'t>

module VectorD =
    let (|Vector|_|) (v: Vector<'t>) : Expr<'t> list option = Some(v.ExprList)
    
    let vec (data:Expr<real list>) = Vector<real> data
    
    let vecz (data:Expr<int list>) = Vector<int> data
    
    let vecq  (data:Expr<rat list>) = Vector<rat> data

    let vecc (data:Expr<complex list>) = Vector<complex> data

    let vvars<'t when 't: equality and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable> s n = vars<'t> s n |> Vector<'t> 
    
    let vexpr(l:Vector<'t>) = l.Expr

    let add (l:Vector<'t>) (r:Vector<'t>) = l + r
    
    let sub (l:Vector<'n, 't>) (r:Vector<'n, 't>) = l - r
    
    let smul (l:'t) (r:Vector<'n, 't>) = Vector<'n, 't>.(*) (l, r)

    let inner_product_val (l:Vector<'n,'t>) (r:Vector<'n,'t>) = (l * r) |> sval
    
    let norm (l:Vector<'n, 't>) =
        let p = l * l in p |> simplify |> call_sqrt |> expand''<'t>  |> Scalar<'t> 

    let euclid_dist (l:Vector<'n, 't>) (r:Vector<'n, 't>) = (l - r) |> norm |> simplify |> Scalar<'t>
