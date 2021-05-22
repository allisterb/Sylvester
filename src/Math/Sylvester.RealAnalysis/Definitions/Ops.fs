namespace Sylvester

open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns

open Sylvester.CAS

type IRealAnalysisSymbolicOps =
    abstract Limit:Expr<'a> -> Expr<'b> -> Expr<'c> -> Expr<'a>
    abstract LimitRight:Expr<'a> -> Expr<'b> -> Expr<'c> -> Expr<'a>
    abstract LimitLeft:Expr<'a> -> Expr<'b> -> Expr<'c> -> Expr<'a>
    abstract Diff:Expr<'a->'b> -> Expr<'a> -> int -> Expr<'a->'b>
    abstract Integrate:Expr<'a->'b> -> Expr<'a> -> Expr<'a->'b>
  
 type MaximaRealAnalysisOps() = 
    interface IRealAnalysisSymbolicOps with
        member __.Limit f x v = Analysis.limit f x v
        member __.LimitRight f x v = Analysis.limit_left f x v
        member __.LimitLeft f x v = Analysis.limit_left f x v
        member __.Diff (f:Expr<'a->'b>) (x:Expr<'a>) n = 
            let vars = param_vars f
            let body = f |> body |> expand''<'b> 
            let i = Analysis.diff body x n
            expand''<'a->'b> (recombine_func vars i)
        member __.Integrate (f:Expr<'a->'b>) (x:Expr<'a>) = 
            let vars = param_vars f
            let body = f |> body |> expand''<'b> 
            let i = Analysis.integrate body x
            expand''<'a->'b> (recombine_func vars i) 

[<AutoOpen>]    
module RealAnalysis =
    do Maxima.init "C:\\MathTools\\maxima-5.44.0\\bin\\maxima.bat"

    let mutable defaultRealAnalysisSymbolicOps = MaximaRealAnalysisOps() :> IRealAnalysisSymbolicOps

 