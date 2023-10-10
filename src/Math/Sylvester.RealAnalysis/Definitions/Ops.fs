namespace Sylvester

open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns

open Sylvester.CAS

type IRealAnalysisSymbolicOps =
    abstract AlgExpand: Expr<'a> -> Expr<'a>
    abstract RatExpand: Expr<'a> -> Expr<'a>
    abstract RatSimp: Expr<'a> -> Expr<'a>
    abstract PartFracOf: Expr<'a> -> Expr<'a>->Expr<'a>
    abstract TrigSimp: Expr<'a> -> Expr<'a>
    abstract TrigExpand: Expr<'a> -> Expr<'a>
    abstract TrigReduce: Expr<'a> -> Expr<'a>
    abstract Limit: Expr<'b> -> Expr<'c> -> Expr<'a> -> Expr<'a> 
    abstract LimitRight: Expr<'b> -> Expr<'c> -> Expr<'a> -> Expr<'a> 
    abstract LimitLeft: Expr<'b> -> Expr<'c> -> Expr<'a> -> Expr<'a>
    abstract Diff:int -> Expr<real> -> Expr<real> -> Expr<real> 
    abstract Integrate:Expr<real> -> Expr<real> -> Expr<real>
    abstract DefiniteIntegral:Expr<real> -> Expr<'a> -> Expr<'a> -> Expr<real> -> Expr<real> when 'a : comparison
    abstract Sum:Expr<'b>->Expr<int>->Expr<int>->Expr<'a>->Expr<'a>
    abstract SolveFor:Expr<real>->Expr<bool>->Expr<real>
    abstract SolveForPosVars:Expr<real>->Expr<bool>->Expr<real> list
  
 type MaximaRealAnalysisOps() = 
    interface IRealAnalysisSymbolicOps with
        member __.AlgExpand x = Algebra.algexpand x
        member __.RatExpand x = Algebra.ratexpand x
        member __.RatSimp x = Algebra.ratsimp x
        member __.PartFracOf f x = Algebra.partfrac_of f x
        member __.TrigSimp x = Analysis.trigsimp x
        member __.TrigExpand x = Analysis.trigexpand x
        member __.TrigReduce x = Analysis.trigreduce x
        member __.Limit x v f = Analysis.limit x v f
        member __.LimitRight x v f = Analysis.limit_left x v f
        member __.LimitLeft x v f = Analysis.limit_left x v f
        member __.Diff n x (e:Expr<real>) = Analysis.diffn n x e
        member __.Integrate x (f:Expr<real>) = Analysis.integrate x f 
        member __.DefiniteIntegral (x:Expr<real>) (l:Expr<'a>) (u:Expr<'a>) (f:Expr<real>)= 
             Analysis.definite_integral x l u f
        member __.Sum (x:Expr<'b>) (l:Expr<int>) (u:Expr<int>) (expr:Expr<'a>) = 
             Analysis.sum x l u expr
        member __.SolveFor (x:Expr<real>) (e:Expr<bool>) = let s = Algebra.solve_for x [e] in if s.Length = 0 then failwith "no solutions" else List.head s
        member __.SolveForPosVars (x:Expr<real>) (e:Expr<bool>) = Algebra.solve_for_pos_vars x e
             
[<AutoOpen>]    
module RealAnalysis =
    let mutable Ops = MaximaRealAnalysisOps() :> IRealAnalysisSymbolicOps 