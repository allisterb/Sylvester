namespace Sylvester

open System

open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns

open Sylvester.CAS

type IRealAnalysisSymbolicOps =
    abstract Limit:Expr<'a> -> Expr<'b> -> Expr<'c> -> Expr<'a>
    abstract LimitRight:Expr<'a> -> Expr<'b> -> Expr<'c> -> Expr<'a>
    abstract LimitLeft:Expr<'a> -> Expr<'b> -> Expr<'c> -> Expr<'a>
    abstract Diff:Expr<'a->'b> -> int -> Expr<'a->'b>
    abstract Integrate:Expr<'a->'b> -> Expr<'a->'b>
    abstract DefiniteIntegral:Expr<'a->'b> -> Expr<'a> -> Expr<'a> ->Expr<'b> when 'a : comparison
    abstract Sum:Expr<'a>->Expr<'b>->Expr<int>->Expr<int>->Expr<'a>
  
 type MaximaRealAnalysisOps() = 
    interface IRealAnalysisSymbolicOps with
        member __.Limit f x v = Analysis.limit f x v
        member __.LimitRight f x v = Analysis.limit_left f x v
        member __.LimitLeft f x v = Analysis.limit_left f x v
        member __.Diff (f:Expr<'a->'b>) n = 
            let vars = param_vars f
            let var = vars |> List.exactlyOne |> Expr.Var |> expand''<'a> 
            let body = f |> body |> expand''<'b> 
            let i = Analysis.diff body var n
            expand''<'a->'b> (recombine_func vars i)
        member __.Integrate (f:Expr<'a->'b>) = 
             let vars = param_vars f
             let var = vars |> List.exactlyOne |> Expr.Var |> expand''<'a> 
             let body = f |> body |> expand''<'b> 
             let i = Analysis.integrate body var
             expand''<'a->'b> (recombine_func vars i)
        member __.DefiniteIntegral (f:Expr<'a->'b >) (l:Expr<'a>) (u:Expr<'a>) = 
             let vars = param_vars f
             let var = vars |> List.exactlyOne |> Expr.Var |> expand''<'a> 
             let body = f |> body |> expand''<'b> 
             Analysis.definite_integral body var l u
        member __.Sum (expr:Expr<'a>) (x:Expr<'b>) (l:Expr<int>) (u:Expr<int>) = 
             Analysis.sum expr x l u
             
[<AutoOpen>]    
module RealAnalysis =
    do Maxima.init "C:\\MathTools\\maxima-5.44.0\\bin\\maxima.bat"

    let mutable defaultRealAnalysisSymbolicOps = MaximaRealAnalysisOps() :> IRealAnalysisSymbolicOps 