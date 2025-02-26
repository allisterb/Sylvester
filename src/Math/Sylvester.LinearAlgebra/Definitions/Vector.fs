namespace Sylvester

open System
open System.Collections
open System.Collections.Generic
open System.Linq
open FSharp.Quotations

open MathNet.Numerics

type IVector<'t when 't: equality and 't :> ValueType and 't :> IEquatable<'t>> = 
    inherit IPartialShape<dim<1>>
    abstract Expr: Expr<'t>[]

[<StructuredFormatDisplay("{UnicodeDisplay}")>]
type Vector<'t when 't: equality and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t>>
    (e: Expr<'t> array, ?h:TermHistory) = 
    do if e.Length = 0 then failwith "The length of a vector must one or greater."
    let expr = e  |> Array.map expand_as<'t>
    let exprmn = Array.map MathNetExpr.fromQuotation expr
    
    member val Length = e.Length
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
    
    member x.ItemE with get i = e.[i] |> ev

    interface IVector<'t> with
        member val Expr = e
        member val Dims = [| e.Length |]
    
    interface IEnumerable<Expr<'t>> with
        member x.GetEnumerator ()  = (x.Expr |> Array.toSeq).GetEnumerator()
        member x.GetEnumerator () = (x :> IEnumerable<Expr<'t>>).GetEnumerator () :> IEnumerator
    
    interface IEquatable<Vector<'t>> with
          member a.Equals b = a.UnicodeDisplay = b.UnicodeDisplay

    interface IHtmlDisplay with
        member x.Html() =
            match h with
            | Some(TermHistory.BinaryOp(op, l, r)) when l.GetType().IsAssignableFrom(typeof<IHtmlDisplay>) && r.GetType().IsAssignableFrom(typeof<IHtmlDisplay>) ->
                let lv = l :?> IHtmlDisplay
                let rv = r :?> IHtmlDisplay
                "$$" + lv.Html().Replace("$$", "") + " " + op + " " + rv.Html().Replace("$$", "") + "$$"
            | _ ->
                let elems =
                    expr 
                    |> Array.skip 1 
                    |> Array.fold(fun s e -> sprintf "%s \\\\ %s" s (sprinte e)) (sprinte expr.[0]) 
                    |> sprintf "%s"
                "$$ \\begin{pmatrix} " + elems + " \\end{pmatrix} $$"

    interface IWebVisualization with
        member x.Draw(attrs:_) =
            let rt = typeof<real>
            match e.Length, typeof<'t> with
            | 2, rt -> WebVisualization.draw_vec2 attrs e.[0].Raw e.[1].Raw |> draw_board
            | _ -> failwith "unsupported"
            
    interface IHistory with
        member val History = h

    new([<ParamArray>] v:Scalar<'t> array) = Vector(sexprs v)

    new([<ParamArray>] v:'t array) = let expr = v |> Array.map exprv in Vector(expr)

    static member create([<ParamArray>] data: 't array) = Vector<'t>(data)

    static member (+) (l: Vector<'t>, r: Vector<'t>) = 
        let e = Array.map2 call_add l.Expr r.Expr |> Array.map (expand_as >> simplifye) in Vector<'t>(e, BinaryOp("+", l, r))
    
    static member (-) (l: Vector<'t>, r: Vector<'t>) = 
        let e = Array.map2 call_sub l.Expr r.Expr |> Array.map (expand_as >> simplifye) in Vector<'t>(e, BinaryOp("-", l, r))

    static member (*) (l: Vector<'t>, r: Vector<'t>) = 
        let e = 
            Array.zip l.Expr r.Expr 
            |> Array.map(fun(a, b) -> call_mul a b)
            |> Array.reduce (call_add)
            |> expand_as
            |> simplifye 
        in Scalar<'t>(e, BinaryOp("*", l, r))

    static member (*) (l: Scalar<'t>, r: Vector<'t>) = 
        r.Expr |> Array.map(fun e -> call_mul (l.Expr) e  |> expand_as<'t> |> simplifye) |> Vector<'t>

    static member (*) (l: Vector<'t>, r: Scalar<'t>) = 
        l.Expr |> Array.map(fun e -> call_mul e (r.Expr) |> expand_as<'t> |> simplifye) |> Vector<'t>

    static member (*) (l: Vector<'t>, r: 't) : Vector<'t> = let r' = Scalar<'t>(exprv r) in l * r' 

    static member (*) (l: 't, r: Vector<'t>) : Vector<'t> = let l' = Scalar<'t>(exprv l) in l' * r

    static member (~-) (l: Vector<'t>) =
        l.Expr |> Array.map(call_neg >> expand_as<'t> >> simplifye) |> Vector<'t>

type Vec = Vector<real>

module Vector =
    let vec (data:obj list) = data |> List.toArray |> realterms |> Vec 
    
    let vexpr(v: Vector<'t>) = v.Expr

    let vdot (l:IVector<'t>) (r:IVector<'t>) =
        do if l.Dims.[0] <> r.Dims.[0] then failwithf "Cannot find dot product of two vectors of different lengths: %A and %A." l.Dims.[0] r.Dims.[0]
        let e = 
            Array.map2 call_mul l.Expr r.Expr 
            |> Array.reduce call_add
            |> expand_as<'t>
            |> simplifye
        in Scalar<'t>(e, BinaryOp("*", l, r))
    
    let vsmul (l:IVector<'t>) (r:Scalar<'t>) =
         l.Expr |> Array.map(fun e -> call_mul e (r.Expr) |> expand_as<'t> |> simplifye) |> Vector<'t>

