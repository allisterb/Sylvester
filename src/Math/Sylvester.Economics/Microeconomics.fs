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

type UtilityFunction(f:RealFunction) =
    inherit RealFunction(f.MapExpr)
    let _rf = f :> IRealFunction<RealFunction>
    interface IRealFunction<UtilityFunction> with
        member x.Term = x
        member x.Expr = x.Body
        member x.Attrs = x.Attrs
        member x.Symbol = x.Symbol
        member x.Transform(b:Expr<real>, ?attrs, ?s) = _rf.Transform(b, ?a=attrs, ?s=s) |> UtilityFunction
        member x.ScalarExpr = Scalar<real> x.Body
        member x.Vars = _rf.Vars
        member x.Html() = _rf.Html()

type UtilityFunction2(f:RealFunction2) =
    inherit RealFunction2(f.MapExpr)
    let _rf = f :> IRealFunction<RealFunction2>
    interface IRealFunction<UtilityFunction2> with
        member x.Term = x
        member x.Expr = x.Body
        member x.Attrs = x.Attrs
        member x.Symbol = x.Symbol
        member x.Transform(b:Expr<real>, ?attrs, ?s) = _rf.Transform(b, ?a=attrs, ?s=s) |> UtilityFunction2
        member x.ScalarExpr = Scalar<real> x.Body
        member x.Vars = _rf.Vars
        member x.Html() = _rf.Html()

type DemandFunction(f:RealFunction) =
    inherit RealFunction(f.MapExpr)
    let _rf = f :> IRealFunction<RealFunction>
    interface IRealFunction<DemandFunction> with
        member x.Term = x
        member x.Expr = x.Body
        member x.Attrs = x.Attrs
        member x.Symbol = x.Symbol
        member x.Transform(b:Expr<real>, ?attrs, ?s) = _rf.Transform(b, ?a=attrs, ?s=s) |> DemandFunction
        member x.ScalarExpr = Scalar<real> x.Body
        member x.Vars = _rf.Vars
        member x.Html() = _rf.Html()
    interface IWebVisualization with
        member x.Draw(attrs:_) =             
            let xxqxx = realvar "xxqxx"
            let eq = xxqxx == Scalar<real> x.Body
            let e = Ops.SolveForPosVars x.ArgExpr [eq.Expr]
            let fe = recombine_func_as<real->real> [xxqxx.Var] e.Head
            WebVisualization.draw_realfun2 attrs ((x :> IRealFunction<DemandFunction>).Html()) fe |> draw_board
            
type InverseDemandFunction(f:RealFunction) =
    inherit RealFunction(f.MapExpr)
    let _rf = f :> IRealFunction<RealFunction>
    interface IRealFunction<InverseDemandFunction> with
        member x.Term = x
        member x.Expr = x.Body
        member x.Attrs = x.Attrs
        member x.Symbol = x.Symbol
        member x.Transform(b:Expr<real>, ?attrs, ?s) = _rf.Transform(b, ?a=attrs, ?s=s) |> InverseDemandFunction
        member x.ScalarExpr = Scalar<real> x.Body
        member x.Vars = _rf.Vars
        member x.Html() = _rf.Html()

type DemandFunction2(f:RealFunction2) =
    inherit RealFunction2(f.MapExpr)
    let _rf = f :> IRealFunction<RealFunction2>
    interface IRealFunction<DemandFunction2> with
        member x.Term = x
        member x.Expr = x.Body
        member x.Attrs = x.Attrs
        member x.Symbol = x.Symbol
        member x.Transform(b:Expr<real>, ?attrs, ?s) = _rf.Transform(b, ?a=attrs, ?s=s) |> DemandFunction2
        member x.ScalarExpr = Scalar<real> x.Body
        member x.Vars = _rf.Vars
        member x.Html() = _rf.Html()

type SupplyFunction(f:RealFunction) =
    inherit RealFunction(f.MapExpr)
    let _rf = f :> IRealFunction<RealFunction>
    interface IRealFunction<SupplyFunction> with
        member x.Term = x
        member x.Expr = x.Body
        member x.Attrs = x.Attrs
        member x.Symbol = x.Symbol
        member x.Transform(b:Expr<real>, ?attrs, ?s) = _rf.Transform(b, ?a=attrs, ?s=s) |> SupplyFunction
        member x.ScalarExpr = Scalar<real> x.Body
        member x.Vars = _rf.Vars
        member x.Html() = _rf.Html()
  
type SupplyFunction2(f:RealFunction2) =
    inherit RealFunction2(f.MapExpr)
    let _rf = f :> IRealFunction<RealFunction2>
    interface IRealFunction<SupplyFunction2> with
        member x.Term = x
        member x.Expr = x.Body
        member x.Attrs = x.Attrs
        member x.Symbol = x.Symbol
        member x.Transform(b:Expr<real>, ?attrs, ?s) = _rf.Transform(b, ?a=attrs, ?s=s) |> SupplyFunction2
        member x.ScalarExpr = Scalar<real> x.Body
        member x.Vars = _rf.Vars
        member x.Html() = _rf.Html()

type ProductionFunction(f:RealFunction) =
    inherit RealFunction(f.MapExpr)
    let _rf = f :> IRealFunction<RealFunction>
    interface IRealFunction<ProductionFunction> with
        member x.Term = x
        member x.Expr = x.Body
        member x.Attrs = x.Attrs
        member x.Symbol = x.Symbol
        member x.Transform(b:Expr<real>, ?attrs, ?s) = _rf.Transform(b, ?a=attrs, ?s=s) |> ProductionFunction
        member x.ScalarExpr = Scalar<real> x.Body
        member x.Vars = _rf.Vars
        member x.Html() = _rf.Html()
        
type ProductionFunction2(f:RealFunction2) =
    inherit RealFunction2(f.MapExpr)
    let _rf = f :> IRealFunction<RealFunction2>
    interface IRealFunction<ProductionFunction2> with
        member x.Term = x
        member x.Expr = x.Body
        member x.Attrs = x.Attrs
        member x.Symbol = x.Symbol
        member x.Transform(b:Expr<real>, ?attrs, ?s) = _rf.Transform(b, ?a=attrs, ?s=s) |> ProductionFunction2
        member x.ScalarExpr = Scalar<real> x.Body
        member x.Vars = _rf.Vars
        member x.Html() = _rf.Html()

type CostFunction(f:RealFunction) =
    inherit RealFunction(f.MapExpr)
    let _rf = f :> IRealFunction<RealFunction>
    interface IRealFunction<CostFunction> with
        member x.Term = x
        member x.Expr = x.Body
        member x.Attrs = x.Attrs
        member x.Symbol = x.Symbol
        member x.Transform(b:Expr<real>, ?attrs, ?s) = _rf.Transform(b, ?a=attrs, ?s=s) |> CostFunction
        member x.ScalarExpr = Scalar<real> x.Body
        member x.Vars = _rf.Vars
        member x.Html() = _rf.Html()
        
type CostFunction2(f:RealFunction2) =
    inherit RealFunction2(f.MapExpr)
    let _rf = f :> IRealFunction<RealFunction2>
    interface IRealFunction<CostFunction2> with
        member x.Term = x
        member x.Expr = x.Body
        member x.Attrs = x.Attrs
        member x.Symbol = x.Symbol
        member x.Transform(b:Expr<real>, ?attrs, ?s) = _rf.Transform(b, ?a=attrs, ?s=s) |> CostFunction2
        member x.ScalarExpr = Scalar<real> x.Body
        member x.Vars = _rf.Vars
        member x.Html() = _rf.Html()

type RevenueFunction(f:RealFunction) =
    inherit RealFunction(f.MapExpr)
    let _rf = f :> IRealFunction<RealFunction>
    interface IRealFunction<RevenueFunction> with
        member x.Term = x
        member x.Expr = x.Body
        member x.Attrs = x.Attrs
        member x.Symbol = x.Symbol
        member x.Transform(b:Expr<real>, ?attrs, ?s) = _rf.Transform(b, ?a=attrs, ?s=s) |> RevenueFunction
        member x.ScalarExpr = Scalar<real> x.Body
        member x.Vars = _rf.Vars
        member x.Html() = _rf.Html()

type PPF(c: EconomicConstraint list) =
    member val Constraints = c

module Microeconomics =
    let solve_for_econ_var (x:realvar) (e:ScalarEquation<real> list) = 
           Ops.SolveForPosVars x.Expr (e |> List.map sexpr) |> List.map(fun v -> ScalarVarMap<real>(x, Scalar<real> v))

    let solve_for_econ_var_unique (x:realvar) (e:ScalarEquation<real> list) =
        let s = solve_for_econ_var x e
        if s.Length > 1 then failwithf "The equation %A has more than 1 solution for %A." e x
        s.[0].Rhs

    let marginal (x:realvar) (func:IRealFunction<'a>)  = 
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

    let demandfun s (func:Scalar<real>) = RealFunction(func, s) |> DemandFunction

    let supplyfun s (func:Scalar<real>) = RealFunction(func, s) |> SupplyFunction

    let utilfun s (func:Scalar<real>) = RealFunction(func, s) |> UtilityFunction
    
    let utilfun2 s (func:Scalar<real>) = RealFunction2(func, s) |> UtilityFunction2

    let prodfun s (func:Scalar<real>) = RealFunction(func, s) |> ProductionFunction

    let prodfun2 s (func:Scalar<real>) = RealFunction2(func, s) |> ProductionFunction2

    let costfun s (func:Scalar<real>) = RealFunction(func, s) |> CostFunction

    let demandfun_im s (x:realvar) (e:ScalarEquation<real>) = realfun_im_pos_vars s x e |> DemandFunction

    let supplyfun_im s (x:realvar) (e:ScalarEquation<real>) = realfun_im_pos_vars s x e |> SupplyFunction

    let utilfun_im s (x:realvar) (e:ScalarEquation<real>) = realfun_im_pos_vars s x e |> UtilityFunction

    let prodfun_im s (x:realvar) (e:ScalarEquation<real>) = realfun_im_pos_vars s x e |> ProductionFunction

    let fail_if_not_equality_constraint (c:EconomicConstraint) = 
        match c.Expr with
        | SpecificCall <@@ (=) @@> (_,_,_::_::[]) -> ()  
        | _ -> failwith "Thi" 

    let inv_demandfun (sym:string) (q:realvar) (f:DemandFunction) =
        let p = farg f
        let s = solve_for_econ_var_unique p [(q == f.[p])] 
        realfun sym s |> InverseDemandFunction

    let revenuefun (s: string) (f:InverseDemandFunction) =
        let q = farg f
        realfun s (q * f.[q]) |> RevenueFunction

    let budget_constraint (r:ScalarEquation<real>) = EconomicConstraint r |> with_attr_tag "BudgetConstraint"

    let ppf (c:ScalarRelation<real> list) = c |> List.map EconomicConstraint |> PPF

    let isoquants (attrs:'a) (dv:realvar) (f:DemandFunction2) (vals:seq<real>) =
        let fs = vals |> Seq.map(fun v -> prodfun_im (sprintf "%s = %A" dv.Name v) dv (f == v) :> IRealFunction<RealFunction>) 
        draw attrs <| realfungrpv fs

    let indifference_curves (attrs:'a) (dv:realvar) (f:RealFunction2) (vals:seq<real>) =
        let fs = vals |> Seq.map(fun v -> utilfun_im (sprintf "%s = %A" dv.Name v) dv (f == v) :> IRealFunction<RealFunction>) 
        draw attrs <| realfungrpv fs

    let constrained_indifference_curves (attrs:'a) (dv:realvar) (f:RealFunction2) (c:EconomicConstraint) (vals:seq<real>) =
        fail_if_not_equality_constraint c
        let fs = vals |> Seq.map(fun v -> utilfun_im (sprintf "%s = %A" dv.Name v) dv (f == v)) |> Seq.append([|realfun_im "" dv (c.Lhs == c.Rhs) |> UtilityFunction|]) |> Seq.cast<IRealFunction<RealFunction>>
        draw attrs <| realfungrpv fs

    let mrs (f:RealFunction2) =
        let M1 = partdiffn 0 f
        let M2 = partdiffn 1 f
        -1 * (M1 / M2) |> ratsimp

    let price_elasticity_demand (f:RealFunction) =
        let p = farg f in elasticity p f