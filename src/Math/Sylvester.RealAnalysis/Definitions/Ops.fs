namespace Sylvester

open System
open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns

open Sylvester.CAS

type IRealAnalysisSymbolicOps =
    abstract AlgExpand: Expr<'a> -> Expr<'a>
    abstract RatExpand: Expr<'a> -> Expr<'a>
    abstract RatSimp: Expr<'a> -> Expr<'a>
    abstract Factor: Expr<'a> -> Expr<'a>
    abstract FactorFor: Expr<'a> -> Expr<'a> -> Expr<'a>
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
    abstract SolveFor:Expr<real>->Expr<bool> list->Expr<bool> list
    abstract SolveForPosVars:Expr<real>->Expr<bool> list->Expr<bool> list
  
 type MaximaRealAnalysisOps() = 
    interface IRealAnalysisSymbolicOps with
        member __.AlgExpand x = Algebra.algexpand x
        member __.RatExpand x = Algebra.ratexpand x
        member __.RatSimp x = Algebra.ratsimp x
        member __.Factor x = Algebra.factor x
        member __.FactorFor p x = Algebra.factor_for p x
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
        member __.SolveFor (x:Expr<real>) (e:Expr<bool> list) = Algebra.solve_for {||} [x] e
        member __.SolveForPosVars (x:Expr<real>) (e:Expr<bool> list) = Algebra.solve_for {||} [x] e
             
[<AutoOpen>]    
module RealAnalysisOps =
    let mutable Ops = MaximaRealAnalysisOps() :> IRealAnalysisSymbolicOps 

    let DefaultZ3Solver = new Z3Solver()

    let solve (o:'a) (v:seq<realvar>)  (eqns: ScalarEquation<real> list) =
        let _eqns = eqns |> List.map(fix_eqn o)
        let vars = v |> Seq.map(fun _v -> _v.Var |> exprvar<real>) |> Seq.toList
        Algebra.solve_for o vars (_eqns |> List.map sexpr) |> List.map scalar_varmap<real>
        
    let solve_unique (o:'a) (x:realvar) (e:ScalarEquation<real> list) =
        let s = solve o [x] e
        if s.Length > 1 then failwithf "The equation %A has more than 1 solution for %A." e x
        s.[0].Rhs

    let eliminate (o:'a) (v:seq<realvar>)  (eqns: ScalarEquation<real> list) =
        let _eqns = eqns |> List.map(fix_eqn o)
        let vars = v |> Seq.map(fun _v -> _v.Var |> exprvar<real>) |> Seq.toList
        Algebra.eliminate o vars (_eqns |> List.map sexpr) |> List.map Scalar

    let solve_for_elim (v:realvar) (e:realvar list) (eqns: ScalarEquation<real> list) =
         let vars = e@[v] |> Seq.map(fun _v -> _v.Var |> exprvar<real>) |> Seq.toList
         Algebra.eliminate defaults vars (eqns |> List.map sexpr) |> List.map Scalar 

    let solve_for_elim_single (v:realvar) (e:realvar list) (eqns: ScalarEquation<real> list) =
        match solve_for_elim v e eqns with
        | s::[] -> s
        | [] -> failwithf "Could not solve the specified equation system for %A by eliminating %A" v e
        | l -> failwithf "Multiple expressions returned attemting to solve the specified equation system for %A by eliminating %A: %A" v e l

    let solve_for (v:realvar) (e:realvar list) (eqns: ScalarEquation<real> list) =
        let allvars = List.map (sexpr >> get_vars) eqns |> List.concat |> List.map (exprvar<real> >> ScalarVar<real>) |> List.distinct
        let evars = List.except (e@[v]) allvars
        let vars = evars@[v] |> Seq.map sexpr |> Seq.toList
        Algebra.eliminate (defaults) vars (eqns |> List.map sexpr) |> List.map Scalar 

    let solve_for_single (v:realvar) (e:realvar list) (eqns: ScalarEquation<real> list) =
           match solve_for v e eqns with
           | s::[] -> s
           | [] -> failwithf "Could not solve the specified equation system for %A in terms of %A" v e
           | l -> failwithf "Multiple expressions returned attempting to solve the specified equation system for %A in terms of %A: %A" v e l
    
    let collect_terms (term:Scalar<real>) (expr:Scalar<real>) = Algebra.collectterms term.Expr expr.Expr |> Scalar