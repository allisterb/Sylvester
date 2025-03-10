namespace Sylvester

open System
open FSharp.Quotations

open MathNet.Numerics.LinearAlgebra

type Expr' = MathNet.Symbolics.Expression

type IVectorSymbolicOps =
    abstract Add:Expr<'t> array -> Expr<'t> array -> Expr<'t> array
    abstract Subtract:Expr<'t> array -> Expr<'t> array -> Expr<'t> array
    abstract InnerProduct:Expr<'t> array -> Expr<'t> array -> Expr<'t>
    
type ILinearAlgebraSymbolicOps =
    inherit IVectorSymbolicOps

module Array2D = 
    let toJagged<'a> (arr: 'a[,]) : 'a [][] = 
        [| for x in 0 .. Array2D.length1 arr - 1 do
               yield [| for y in 0 .. Array2D.length2 arr - 1 -> arr.[x, y] |]
            |]
            
    let transpose (mtx : _ [,]) = Array2D.init (mtx.GetLength 1) (mtx.GetLength 0) (fun x y -> mtx.[y,x])

    let flatten (arr:_[,]) =  
        seq {for i in 0..Array2D.length1 arr - 1 do
                for j in 0..Array2D.length2 arr - 1 do yield arr.[i,j]} |> Seq.toArray

    let forall (pred:int->int->_->bool) (arr:_[,]) =  
        seq {for i in 0..Array2D.length1 arr - 1 do
                for j in 0..Array2D.length2 arr - 1 do yield pred i j arr.[i,j]} 
        |> Seq.forall((=) true)

module LinearAlgebraOps =
    let vars (a:Expr<_> array list) = a |> List.map Array.toList |> List.concat |> List.map get_vars |> List.concat

    let exprs (a:Expr<_> array list) = a |> List.map Array.toList |> List.concat |> List.map MathNetExpr.fromQuotation
    
    let fromQ e = Array.map MathNetExpr.fromQuotation e

    let toQ<'t> v e = MathNetExpr.toQuotation<'t> v e

    let mat_to_array (a:Expr<'t> list list) = (List.map(List.toArray) >> List.toArray) a
    
    let mat_to_list (a:Expr<'t> array array) = (Array.map(Array.toList) >> Array.toList) a

    let identity_mat<'t> i  = Array2D.init i i (krdelta<'t>) |> Array2D.map(expand_as<'t>) |> Array2D.toJagged

    /// Based on: http://www.fssnip.net/aD/title/Matrix
    let transpose_mat matrix =
        let rec fetch_column acc (matr:(Expr<'t> list list)) = (* Makes a column list from a row list *)
            if matr.Head.Length = 0 then (List.rev acc) (* Stop *)
            else fetch_column
                    ([for row in matr -> row.Head]::acc) (* Fetches the first item from each row *)
                    (List.map (fun row -> match row with [] -> [] | h::t -> t) matr)
        fetch_column [] (matrix |> (Array.map(Array.toList) >> Array.toList)) |> (List.map(List.toArray) >> List.toArray)

    let count_by (f : ('a -> bool)) (array : 'a[]) : int =
        Array.fold(fun acc item -> if f item then acc + 1 else acc) 0 array
    
    let maxi(arr:'a[]) = arr |> Array.indexed |> Array.maxBy snd


module AlgebraOps = 

    let algexpand (x:ISymbolic<_, 't>) = x |> sexpr |> CAS.Algebra.algexpand |> x.Transform

    let ratexpand (x:ISymbolic<_, 't>) = x |> sexpr |> CAS.Algebra.ratexpand |> x.Transform
      
    let ratsimp (x:ISymbolic<_, real>) = x |> sexpr |> CAS.Algebra.ratsimp |> x.Transform

    let factor (x:ISymbolic<_, real>) = x |> sexpr |> CAS.Algebra.factor |> x.Transform

    let factor_for (p:Scalar<real>) (x:ISymbolic<_, real>) = x |> sexpr |> CAS.Algebra.factor_for p.Expr |> x.Transform

type DefaultLinearAlgebraSymbolic() =
    interface ILinearAlgebraSymbolicOps with
        member x.Add l r = Array.map2 call_add l r |> Array.map (expand_as >> simplifye)        
        member x.Subtract l r = Array.map2 call_sub l r |> Array.map (expand_as >> simplifye)
        member x.InnerProduct l r =                     
            Array.zip l r 
            |> Array.map(fun(a, b) -> call_mul a b)
            |> Array.reduce (call_add)
            |> expand_as
            |> simplifye
            
[<AutoOpen>]
module LinearAlgbra =
    let (|LinearAlgebraNumericOpType|_|):Type->unit option =
        function
        | t when t.Name = "Single" || t.Name = "Double" || t.Name = "Complex" || t.Name = "Complex32" -> Some()
        | _ -> None

    let mutable defaultLinearAlgebraSymbolicOps = new DefaultLinearAlgebraSymbolic() :> IVectorSymbolicOps

 