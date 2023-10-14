namespace Sylvester

open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns
 
type EconomicConstraint(lhs, rhs, op) = 
    inherit ScalarRelation<real>(lhs, rhs, op) 
    do 
        match base.Expr with
        | SpecificCall <@@ (=) @@> (_,_,lhs::rhs::[]) 
        | SpecificCall <@@ (>) @@> (_,_,lhs::rhs::[])
        | SpecificCall <@@ (<) @@> (_,_,lhs::rhs::[]) -> ()
        | _ -> failwithf "The expression %s is not a valid constraint expression." (src base.Expr) 
    new(r:ScalarRelation<real>) = 
        EconomicConstraint(r.Lhs, r.Rhs, r.Op)
         
type PPF(c: EconomicConstraint list) =
    member val Constraints = c

module Microeconomics =
    let solve_for_econ_var (x:realvar) (e:ScalarEquation<real> list) = 
           Ops.SolveForPosVars x.Expr (e |> List.map sexpr) |> List.map(fun v -> ScalarVarMap<real>(x, Scalar<real> v))

    let solve_for_econ_var_unique (x:realvar) (e:ScalarEquation<real> list) =
        let s = solve_for_econ_var x e
        if s.Length > 1 then failwithf "The equation %A has more than 1 solution for %A." e x
        s.[0].Rhs

    let inline marginal (x:realvar) (func:IRealFunction<'a>)  = 
        match func.Symbol with
        | None -> diff x func |> with_attr_tag "Marginal" 
        | Some s -> diff x func |> with_attr_tag "Marginal" |> with_symbol ("M" + s.JoinSuperscript(x.Name))

    let elasticity (x:realvar) (func:IRealFunction<'a>) =
        let d = diffs x func
        let s = d * (x / func.ScalarExpr)
        let f = func.Transform(s.Expr)
        f

    let average (func:RealFunction) = 
        match func.Symbol with
        | None -> RealFunction(fexpr func / farg func) |> with_attr_tag "Average"
        | Some s -> diff (farg func) func |> with_attr_tag "Average" |> with_symbol ("A" + s)

    let demandfun s (func:Scalar<real>) :RealFunction = RealFunction(func, s) |> with_attr_tag "DemandFunction"

    let supplyfun s (func:Scalar<real>) :RealFunction = RealFunction(func, s) |> with_attr_tag "SupplyFunction"

    let utilfun s (func:Scalar<real>) :RealFunction = RealFunction(func, s) |> with_attr_tag "UtilityFunction"
    
    let utilfun2 s (func:Scalar<real>) :RealFunction2 = RealFunction2(func, s) |> with_attr_tag "UtilityFunction"

    let prodfun s (func:Scalar<real>) :RealFunction = RealFunction(func, s) |> with_attr_tag "ProductionFunction"

    let prodfun2 s (func:Scalar<real>) :RealFunction2 = RealFunction2(func, s) |> with_attr_tag "ProductionFunction"

    let costfun s (func:Scalar<real>) :RealFunction = RealFunction(func, s) |> with_attr_tag "CostFunction"

    let demandfun_im s (x:realvar) (e:ScalarEquation<real>) :RealFunction = realfun_im_pos_vars s x e |> with_attr_tag "DemandFunction"

    let supplyfun_im s (x:realvar) (e:ScalarEquation<real>) :RealFunction = realfun_im_pos_vars s x e |> with_attr_tag "SupplyFunction"

    let utilfun_im s (x:realvar) (e:ScalarEquation<real>) :RealFunction = realfun_im_pos_vars s x e |> with_attr_tag "UtilityFunction"

    let prodfun_im s (x:realvar) (e:ScalarEquation<real>) :RealFunction = realfun_im_pos_vars s x e |> with_attr_tag "ProductionFunction"

    let fail_if_not_demandfun (f:IRealFunction<_>) =  if not <| has_attr_tag "DemandFunction" f then failwithf "The function %A is not a demand function." f
    
    let fail_if_not_supplyfun (f:IRealFunction<_>) =  if not <| has_attr_tag "SupplyFunction" f then failwithf "The function %A is not a supply function." f

    let fail_if_not_utilfun (f:IRealFunction<_>) =  if not <| has_attr_tag "UtilityFunction" f then failwithf "The function %A is not a utility function." f

    let fail_if_not_prodfun (f:IRealFunction<_>) =  if not <| has_attr_tag "ProductionFunction" f then failwithf "The function %A is not a production function." f
    
    let fail_if_not_costfun (f:IRealFunction<_>) =  if not <| has_attr_tag "CostFunction" f then failwithf "The function %A is not a cost function." f

    let fail_if_not_inv_demandfun (f:IRealFunction<_>) =  if not <| has_attr_tag "InverseDemandFunction" f then failwithf "The function %A is not an inverse demand function." f

    let fail_if_not_equality_constraint (c:EconomicConstraint) = 
        match c.Expr with
        | SpecificCall <@@ (=) @@> (_,_,_::_::[]) -> ()  
        | _ -> failwith "Thi" 

    let inv_demandfun (sym:string) (q:realvar) (f:RealFunction) =
        do fail_if_not_demandfun f
        let p = farg f
        let s = solve_for_econ_var_unique p [(q == f.[p])] 
        realfun sym s |> with_attr_tag "InverseDemandFunction"

    let revenuefun (s: string) (f:RealFunction) =
        do fail_if_not_inv_demandfun f
        let q = farg f
        realfun s (q * f.[q]) |> with_attr_tag "RevenueFunction"

    let budget_constraint (r:ScalarEquation<real>) = EconomicConstraint r |> with_attr_tag "BudgetConstraint"

    let ppf (c:ScalarRelation<real> list) = c |> List.map EconomicConstraint |> PPF

    let isoquants (attrs:'a) (dv:realvar) (f:RealFunction2) (vals:real[]) =
        do fail_if_not_demandfun f
        let fs = vals |> Array.map(fun v -> prodfun_im (sprintf "%s = %A" dv.Name v) dv (f == v)) 
        draw attrs <| realfungrp fs

    let indifference_curves (attrs:'a) (dv:realvar) (f:RealFunction2) (vals:real[]) =
        let fs = vals |> Array.map(fun v -> utilfun_im (sprintf "%s = %A" dv.Name v) dv (f == v)) 
        draw attrs <| realfungrp fs

    let constrained_indifference_curves (attrs:'a) (dv:realvar) (f:RealFunction2) (c:EconomicConstraint) (vals:real[]) =
        fail_if_not_equality_constraint c
        let fs = vals |> Array.map(fun v -> utilfun_im (sprintf "%s = %A" dv.Name v) dv (f == v)) |> Array.append([|realfun_im "" dv (c.Lhs == c.Rhs)|])
        draw attrs <| realfungrp fs

    let mrs (f:RealFunction2) =
        let M1 = partdiffn 0 f
        let M2 = partdiffn 1 f
        -1 * (M1 / M2) |> ratsimp

    let price_elasticity_demand (f:RealFunction) =
        do fail_if_not_demandfun f
        let p = farg f in elasticity p f