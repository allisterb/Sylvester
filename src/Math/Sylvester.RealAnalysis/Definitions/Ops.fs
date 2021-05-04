namespace Sylvester

open FSharp.Quotations

open Sylvester.CAS

type IRealAnalysisSymbolicOps =
    abstract Limit:Expr<'a> -> Expr<'b> -> Expr<'c> -> Expr<'a>
    abstract LimitRight:Expr<'a> -> Expr<'b> -> Expr<'c> -> Expr<'a>
    abstract LimitLeft:Expr<'a> -> Expr<'b> -> Expr<'c> -> Expr<'a>
    abstract Diff:Expr<'a> -> Expr<'b> -> int -> Expr<'a>
  
 type MaximaRealAnalysisOps() = 
    interface IRealAnalysisSymbolicOps with
        member __.Limit f x v = Analysis.limit f x v
        member __.LimitRight f x v = Analysis.limit_left f x v
        member __.LimitLeft f x v = Analysis.limit_left f x v
        member __.Diff (f:Expr<'a>) (x:Expr<'b>) n = 
            let c = f |> get_vars |> List.except (get_vars x)
            Analysis.diff f x c n


[<AutoOpen>]    
module RealAnalysis =
    do Maxima.init "C:\\MathTools\\maxima-5.44.0\\bin\\maxima.bat"

    let mutable defaultRealAnalysisSymbolicOps = MaximaRealAnalysisOps() :> IRealAnalysisSymbolicOps