namespace Sylvester

open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns
 
type EconomicConstraint(a:ScalarRelation<real>) = 
    do 
        match a.Expr with
        | SpecificCall <@@ (=) @@> (_,_,lhs::rhs::[])
        | SpecificCall <@@ (>) @@> (_,_,lhs::rhs::[])
        | SpecificCall <@@ (<) @@> (_,_,lhs::rhs::[]) -> ()
        | _ -> failwithf "The expression %s is not a valid constraint expression." (src a.Expr)

type PPF(c: EconomicConstraint list) =
    member val Constraints = c

module Microeconomics =
    let inline marginal (x:ScalarVar<real>) (func:IRealFunction<'a>)  = 
        match func.Symbol with
        | None -> diff x func |> with_attr_tag "Marginal" 
        | Some s -> diff x func |> with_attr_tag "Marginal" |> with_symbol ("M" + s.JoinSuperscript(x.Name))

    let average (func:RealFunction) = 
        match func.Symbol with
        | None -> RealFunction((fexpr func / farg func)) |> with_attr_tag "Average"
        | Some s -> diff (farg func) func |> with_attr_tag "Average" |> with_symbol ("A" + s)

    let demandfun s (func:Scalar<real>) :RealFunction = RealFunction(func, s) |> with_attr_tag "DemandFunction"

    let supplyfun s (func:Scalar<real>) :RealFunction = RealFunction(func, s) |> with_attr_tag "SupplyFunction"

    let utilfun s (func:Scalar<real>) :RealFunction = RealFunction(func, s) |> with_attr_tag "UtilityFunction"
    
    let utilfun2 s (func:Scalar<real>) :RealFunction2 = RealFunction2(func, s) |> with_attr_tag "UtilityFunction"

    let prodfun s (func:Scalar<real>) :RealFunction = RealFunction(func, s) |> with_attr_tag "ProductionFunction"

    let prodfun2 s (func:Scalar<real>) :RealFunction2 = RealFunction2(func, s) |> with_attr_tag "ProductionFunction"

    let costfun s (func:Scalar<real>) :RealFunction = RealFunction(func, s) |> with_attr_tag "CostFunction"

    let demandfun_im s (x:ScalarVar<real>) (e:ScalarEquation<real>) :RealFunction = realfun_im_pos_vars s x e |> with_attr_tag "DemandFunction"

    let supplyfun_im s (x:ScalarVar<real>) (e:ScalarEquation<real>) :RealFunction = realfun_im_pos_vars s x e |> with_attr_tag "SupplyFunction"

    let utilfun_im s (x:ScalarVar<real>) (e:ScalarEquation<real>) :RealFunction = realfun_im_pos_vars s x e |> with_attr_tag "UtilityFunction"

    let prodfun_im s (x:ScalarVar<real>) (e:ScalarEquation<real>) :RealFunction = realfun_im_pos_vars s x e |> with_attr_tag "ProductionFunction"

    let ppf (c:ScalarRelation<real> list) = c |> List.map EconomicConstraint |> PPF

    let solve_for_econ_var (x:ScalarVar<real>) (e:ScalarEquation<real>) = 
        Ops.SolveForPosVars x.Expr e.Expr |> List.map(fun v -> ScalarVarMap<real>(x, Scalar<real> v))

    let isoquants (attrs:'a) (dv:ScalarVar<real>) (f:RealFunction2) (vals:real[]) =
        let fs = vals |> Array.map(fun v -> prodfun_im "" dv (f == v)) 
        draw attrs <| realfungrp fs

    let indifference_curves (attrs:'a) (dv:ScalarVar<real>) (f:RealFunction2) (vals:real[]) =
        let fs = vals |> Array.map(fun v -> utilfun_im "" dv (f == v)) 
        draw attrs <| realfungrp fs

    let mrts (f:RealFunction2) =
        let M1 = (marginal (ScalarVar<real> f.ScalarVars.[0].Name) f).ScalarExpr |> Scalar<real>
        let M2 = (marginal (ScalarVar<real> f.ScalarVars.[1].Name) f).ScalarExpr |> Scalar<real>
        -1 * (M1 / M2) |> ratsimp