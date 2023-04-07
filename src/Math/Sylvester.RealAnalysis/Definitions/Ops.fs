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
    abstract Integrate:Expr<real> -> Expr<real> -> Expr<real>
    abstract DefiniteIntegral:Expr<real> -> Expr<'a> -> Expr<'a> -> Expr<real> -> Expr<real> when 'a : comparison
    abstract Sum:Expr<'a>->Expr<'b>->Expr<int>->Expr<int>->Expr<'a>
  
 type MaximaRealAnalysisOps() = 
    interface IRealAnalysisSymbolicOps with
        member __.Limit f x v = Analysis.limit x v f
        member __.LimitRight f x v = Analysis.limit_left x v f
        member __.LimitLeft f x v = Analysis.limit_left x v f
        member __.Diff n x (e:Expr<real>) = Analysis.diffn n x e
        member __.Integrate x (f:Expr<real>) = Analysis.integrate x f 
        member __.DefiniteIntegral (x:Expr<real>) (l:Expr<'a>) (u:Expr<'a>) (f:Expr<real>)= 
             Analysis.definite_integral x l u f
        member __.Sum (expr:Expr<'a>) (x:Expr<'b>) (l:Expr<int>) (u:Expr<int>) = 
             Analysis.sum x l u expr
             
[<AutoOpen>]    
module RealAnalysis =
    let mutable Ops = MaximaRealAnalysisOps() :> IRealAnalysisSymbolicOps 