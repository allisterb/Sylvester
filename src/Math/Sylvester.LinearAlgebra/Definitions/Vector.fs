namespace Sylvester

open System
open System.Collections
open System.Collections.Generic

open FSharp.Quotations
open MathNet.Numerics

open Arithmetic
open Dimension

[<StructuredFormatDisplay("{UnicodeDisplay}")>]
type Vector<'t when 't: equality and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t>>
    internal(e: Expr<'t> array, ?h:TermHistory) = 
    do if e.Length = 0 then failwith "The length of a vector must one or greater."
    let expr = e  |> Array.map expand_as<'t>
    let exprmn = Array.map MathNetExpr.fromQuotation expr
    
    member val Expr = expr
    member val ExprList = expr |> Array.toList
    member val ExprVars = expr |> Array.map (get_vars >> List.toArray) |> Array.concat
    member val ExprMathNet = exprmn

    member val UnicodeDisplay =
           expr 
           |> Array.skip 1 
           |> Array.fold(fun s e -> sprintf "%s, %s" s (sprinte e)) (sprinte expr.[0]) 
           |> sprintf "(%s)"

    member x.AsNumeric() = 
        let t = typeof<'t>
        match t with
        | LinearAlgebraNumericOpType -> expr |> Array.map evaluate |> Array.map (Convert.ToSingle) |> LinearAlgebra.DenseVector.raw
        | _ -> failwithf "The type %A is not compatible with numeric linear algebra operations." t
    
    member x.Item with get i = e.[i] |> Scalar<'t>
    
    interface IPartialShape<``1``> with
        member val Rank = Some 1 with get,set
        member val Dims = [| Convert.ToInt64(e.Length) |] |> Some with get,set
    
    interface IEnumerable<Expr<'t>> with
        member x.GetEnumerator ()  = (x.Expr |> Array.toSeq).GetEnumerator()
        member x.GetEnumerator () = (x :> IEnumerable<Expr<'t>>).GetEnumerator () :> IEnumerator
    
    interface IEquatable<Vector<'t>> with
          member a.Equals b = a.UnicodeDisplay = b.UnicodeDisplay

    interface IHtmlDisplay with
        member x.Html() =
            let elems =
                expr 
                |> Array.skip 1 
                |> Array.fold(fun s e -> sprintf "%s \\\\ %s" s (sprinte e)) (sprinte expr.[0]) 
                |> sprintf "%s"
            "$$ \\begin{pmatrix} " + elems + " \\end{pmatrix} $$"

    interface IWebVisualization with
        member x.Draw(attrs:obj) =
            <@ 4 @> |> draw_board

    interface IHistory with
        member val History = h

    new([<ParamArray>] v:Scalar<'t> array) = Vector(sexprs v)

    new([<ParamArray>] v:'t array) = let expr = v |> Array.map exprv in Vector(expr)
    
    static member (+) (l: Vector<'t>, r: Vector<'t>) = 
        Array.map2 call_add l.Expr r.Expr |> Array.map (expand_as >> simplifye) |> Vector<'t>

    static member (-) (l: Vector<'t>, r: Vector<'t>) = 
        Array.map2 call_sub l.Expr r.Expr |> Array.map (expand_as >> simplifye) |> Vector<'t>


    static member create([<ParamArray>] data: 't array) = Vector<'t>(data)

[<StructuredFormatDisplay("{UnicodeDisplay}")>]
type Vector<'dim0, 't when 'dim0 :> Number and 't: equality and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable>
    internal (e: Expr<'t> array, ?h:TermHistory) =
    inherit Vector<'t>(e, ?h=h)
    let dim0 = number<'dim0>
    do if e.Length <> dim0.IntVal then failwithf "The initializing array has length %i instead of %i." e.Length dim0.IntVal
    
    member val Dim0:'dim0 = dim0
    
    member x.Norm = let p = x * x in p |> simplify |> call_sqrt |> expand_as<'t>
        
    new([<ParamArray>] v:Scalar<'t> array) = let expr = v |> Array.map sexpr in Vector<'dim0, 't>(expr)

    new([<ParamArray>] v:'t array) = let expr = v |> Array.map exprv in Vector<'dim0, 't>(expr)
    
    interface IVector<'dim0> with member val Dim0 = dim0

    interface IEquatable<Vector<'dim0, 't>> with
        member a.Equals b = a.UnicodeDisplay = b.UnicodeDisplay
     
    static member Zero:Vector<'dim0, 't> = let e = Array.create number<'dim0>.IntVal (zero_val(typeof<'t>) |> expand_as<'t>) in Vector<'dim0, 't> e

    static member One:Vector<'dim0, 't> = let e = Array.create number<'dim0>.IntVal (one_val(typeof<'t>) |> expand_as<'t>) in Vector<'dim0, 't> e

    static member (+) (l: Vector<'dim0, 't>, r: Vector<'dim0, 't>) = 
        let e = Array.map2 call_add l.Expr r.Expr |> Array.map (expand_as >> simplifye) in Vector<'dim0, 't>(e, BinaryOp("+", l, r))
    
    static member (-) (l: Vector<'dim0, 't>, r: Vector<'dim0, 't>) = 
        let e = Array.map2 call_sub l.Expr r.Expr |> Array.map (expand_as >> simplifye) in Vector<'dim0, 't>(e, BinaryOp("-", l, r))

    static member (*) (l: Vector<'dim0, 't>, r: Vector<'dim0, 't>) = 
        let e = 
            Array.zip l.Expr r.Expr 
            |> Array.map(fun(a, b) -> call_mul a b)
            |> Array.reduce (call_add)
            |> expand_as
            |> simplifye 
        in Scalar<'t>(e, BinaryOp("*", l, r))

    static member (*) (l: Scalar<'t>, r: Vector<'dim0, 't>) = 
        r.Expr |> Array.map(fun e -> call_mul (l.Expr) e |> expand_as<'t> |> simplifye) |> Vector<'n, 't>

    static member (*) (l: Vector<'dim0, 't>, r: Scalar<'t>) = 
        l.Expr |> Array.map(fun e -> call_mul e (r.Expr) |> expand_as<'t> |> simplifye) |> Vector<'n, 't>

    static member (*) (l: Vector<'dim0, 't>, r: 't) : Vector<'dim0, 't> = let r' = Scalar<'t>(exprv r) in l * r' 

    static member (*) (l: 't, r: Vector<'dim0, 't>) : Vector<'dim0, 't> = let l' = Scalar<'t>(exprv l) in l' * r

    static member (~-) (l: Vector<'dim0, 't>) =
        l.Expr |> Array.map(call_neg >> expand_as<'t> >> simplifye) |> Vector<'n, 't>

type Vec<'dim0 when 'dim0 :> Number> = Vector<'dim0, real>
type VecC<'dim0 when 'dim0 :> Number> = Vector<'dim0, complex>
type VecQ<'dim0 when 'dim0 :> Number> = Vector<'dim0, rat>
type VecZ<'dim0 when 'dim0 :> Number> = Vector<'dim0, int>

module Vector =
    let (|Vector|_|) (v: Vector<'n, 't>) : Expr<'t> list option = Some(v.ExprList)
    
    let vexpr(v: Vector<'n, 't>) = v.Expr

    let vec (dim:'n) (data:obj list) = data |> List.toArray |> realterms |> Vec<'n> 
    
    //let vecz (dim:'n) (data:Term<int> list) = Vector<'n, int> data
    
    //let vecq  (dim:'n) (data:Term<rat> list) = Vector<'n, rat> data

    let vvars<'n, 't when 'n :> Number and 't: equality and 't: comparison and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable> s = vars<'t> s (number<'n>.IntVal) |> Vector<'n, 't> 
    
    let add (l:Vector<'n, 't>) (r:Vector<'n, 't>) = l + r
    
    let sub (l:Vector<'n, 't>) (r:Vector<'n, 't>) = l - r
    
    let smul (l:'t) (r:Vector<'n, 't>) = Vector<'n, 't>.(*) (l, r)

    let inner_product (l:Vector<'n,'t>) (r:Vector<'n,'t>) = (l * r) 
    
    let norm (l:Vector<'n, 't>) =
        let p = l * l in p |> simplify |> call_sqrt |> expand_as<'t>  |> Scalar

    let euclid_dist (l:Vector<'n, 't>) (r:Vector<'n, 't>) = (l - r) |> norm |> simplify |> Scalar

[<AutoOpen>]
module Vectors =
    
    open Vector
    
    let vec2 x y = vec ``2`` [x; y]
    
    let vec3 x y z = vec ``3`` [x; y; z] 

    let vec4 x y z a = vec ``4`` [x; y; z; a]

    let vec5 x y z a b = vec ``5`` [x; y; z; a; b]
    
    let vec6 x y z a b c = vec ``6`` [x; y; z; a; b; c]
