namespace Sylvester

open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns

open Sylvester.CAS

type IRealAnalysisSymbolicOps =
    abstract Limit:Expr<'a> -> Expr<'b> -> Expr<'c> -> Expr<'a>
    abstract LimitRight:Expr<'a> -> Expr<'b> -> Expr<'c> -> Expr<'a>
    abstract LimitLeft:Expr<'a> -> Expr<'b> -> Expr<'c> -> Expr<'a>
    abstract Diff:int -> Expr<real> -> Expr<real> -> Expr<real>
    abstract Integrate:Expr<real->'b> -> Expr<real> -> Expr<real->'b>
    abstract DefiniteIntegral:Expr<'a->'b> -> Expr<'a> -> Expr<'a> ->Expr<'b> when 'a : comparison
    abstract Sum:Expr<'a>->Expr<'b>->Expr<int>->Expr<int>->Expr<'a>
  
 type MaximaRealAnalysisOps() = 
    interface IRealAnalysisSymbolicOps with
        member __.Limit f x v = Analysis.limit x v f
        member __.LimitRight f x v = Analysis.limit_left x v f
        member __.LimitLeft f x v = Analysis.limit_left x v f
        member __.Diff n x (e:Expr<real>) = Analysis.diffn n x e
        member __.Integrate (f:Expr<real->'b>) x = 
            do if range_type typeof<real->'b> <> typeof<real> then failwithf "The range of the function %s is not real." (sprinte f) 
            let vars = param_vars f
            let b = f |> body  
            let i = Analysis.integrate x <@ %%b:real @> 
            expand''<real->'b> (recombine_func vars i)
        member __.DefiniteIntegral (f:Expr<'a->'b >) (l:Expr<'a>) (u:Expr<'a>) = 
             let vars = param_vars f
             let var = vars |> List.exactlyOne |> Expr.Var |> expand''<'a> 
             let body = f |> body |> expand''<'b> 
             Analysis.definite_integral var l u body
        member __.Sum (expr:Expr<'a>) (x:Expr<'b>) (l:Expr<int>) (u:Expr<int>) = 
             Analysis.sum x l u expr
             
[<AutoOpen>]    
module RealAnalysis =
    let mutable Ops = MaximaRealAnalysisOps() :> IRealAnalysisSymbolicOps 