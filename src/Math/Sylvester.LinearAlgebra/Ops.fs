namespace Sylvester

open System
open FSharp.Quotations

open MathNet.Numerics.LinearAlgebra

type Expr' = MathNet.Symbolics.Expression

type _Vector<'t when 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable> = Vector<'t>

type _Matrix<'t when 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable> = Matrix<'t>

type IVectorSymbolicOps =
    abstract Add:Expr<'t> array -> Expr<'t> array -> Expr<'t> array
    abstract Subtract:Expr<'t> array -> Expr<'t> array -> Expr<'t> array
    abstract InnerProduct:Expr<'t> array -> Expr<'t> array -> Expr<'t>
    
type ILinearAlgebraSymbolicOps =
    inherit IVectorSymbolicOps
    
module private Ops =
    let vars (a:Expr<_> array list) = a |> List.map Array.toList |> List.concat |> List.map get_vars |> List.concat

    let exprs (a:Expr<_> array list) = a |> List.map Array.toList |> List.concat |> List.map MathNetExpr.fromQuotation
    
    let fromQ e = Array.map MathNetExpr.fromQuotation e

    let toQ<'t> v e = MathNetExpr.toQuotation'<'t> v e

type DefaultLinearAlgebraSymbolic() =
    interface ILinearAlgebraSymbolicOps with
        member x.Add l r = Array.map2 call_add l r |> Array.map (expand'')        
        member x.Subtract l r = Array.map2 call_sub l r |> Array.map expand''
        member x.InnerProduct l r =                     
            Array.zip l r 
            |> Array.map(fun(a, b) -> call_mul a b)
            |> Array.reduce (call_add)
            |> expand''
            
[<AutoOpen>]
module LinearAlgbra =
    let (|LinearAlgebraNumericOpType|_|):Type->unit option =
        function
        | t when t.Name = "Single" || t.Name = "Double" || t.Name = "Complex" || t.Name = "Complex32" -> Some()
        | _ -> None

    let mutable defaultLinearAlgebraSymbolicOps = new DefaultLinearAlgebraSymbolic() :> IVectorSymbolicOps