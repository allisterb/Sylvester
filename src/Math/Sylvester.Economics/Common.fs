namespace Sylvester

open System
open System.Collections.Generic
open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns
 
type UtilityFunction(f:RealFunction) =
    inherit RealFunction(f.ScalarExpr, ?symbol=f.Symbol)
    let _rf = f :> IRealFunction<RealFunction>
    interface IRealFunction<UtilityFunction> with
        member x.Term = x
        member x.Expr = x.ScalarExpr.Expr
        member x.SymbolicExpr = x.SymbolicFn.ScalarExpr.Expr
        member x.Attrs = x.Attrs
        member x.Symbol = x.Symbol
        member x.Transform(b:Expr<real>, ?attrs, ?s) = _rf.Transform(b, ?a=attrs, ?s=s) |> UtilityFunction
        member x.SymbolicFn = RealFunction(defaultArg x.Symbol "f", x.ScalarVars.[0].Name) |> UtilityFunction
        member x.ScalarExpr = x.ScalarExpr
        member x.ScalarVars = _rf.ScalarVars
        member x.Html() = _rf.Html()

type UtilityFunction2(f:RealFunction2) =
    inherit RealFunction2(f.ScalarExpr, ?symbol=f.Symbol)
    let _rf = f :> IRealFunction<RealFunction2>
    interface IRealFunction<UtilityFunction2> with
        member x.Term = x
        member x.Expr = x.ScalarExpr.Expr
        member x.SymbolicExpr = x.SymbolicFn.ScalarExpr.Expr
        member x.Attrs = x.Attrs
        member x.Symbol = x.Symbol
        member x.Transform(b:Expr<real>, ?attrs, ?s) = _rf.Transform(b, ?a=attrs, ?s=s) |> UtilityFunction2
        member x.ScalarExpr = x.ScalarExpr
        member x.ScalarVars = _rf.ScalarVars
        member x.SymbolicFn = RealFunction2(defaultArg x.Symbol "f", x.ScalarVars.[0].Name, x.ScalarVars.[1].Name) |> UtilityFunction2
        member x.Html() = _rf.Html()

type DemandFunction(f:RealFunction) =
    inherit RealFunction(f.ScalarExpr, ?symbol=f.Symbol)
    let _rf = f :> IRealFunction<RealFunction>
    interface IRealFunction<DemandFunction> with
        member x.Term = x
        member x.Expr = x.ScalarExpr.Expr
        member x.SymbolicExpr = x.SymbolicFn.ScalarExpr.Expr
        member x.Attrs = x.Attrs
        member x.Symbol = x.Symbol
        member x.Transform(b:Expr<real>, ?attrs, ?s) = _rf.Transform(b, ?a=attrs, ?s=s) |> DemandFunction
        member x.SymbolicFn = RealFunction(defaultArg x.Symbol "f", x.ScalarVars.[0].Name) |> DemandFunction
        member x.ScalarExpr = x.ScalarExpr
        member x.ScalarVars = _rf.ScalarVars
        member x.Html() = _rf.Html()
    interface IWebVisualization with
        member x.Draw(attrs:_) =             
            let xxqxx = realvar "xxqxx"
            let eq = xxqxx == x.ScalarExpr
            let e = Ops.SolveForPosVars x.ArgExpr [eq.Expr]
            let fe = recombine_func_as<real->real> [xxqxx.Var] e.Head
            WebVisualization.draw_realfun2 attrs ((x :> IRealFunction<DemandFunction>).Html()) fe |> draw_board
            
type InverseDemandFunction(f:RealFunction) =
    inherit RealFunction(f.ScalarExpr, ?symbol=f.Symbol)
    let _rf = f :> IRealFunction<RealFunction>
    interface IRealFunction<InverseDemandFunction> with
        member x.Term = x
        member x.Expr = x.ScalarExpr.Expr
        member x.SymbolicExpr = x.SymbolicFn.ScalarExpr.Expr
        member x.Attrs = x.Attrs
        member x.Symbol = x.Symbol
        member x.Transform(b:Expr<real>, ?attrs, ?s) = _rf.Transform(b, ?a=attrs, ?s=s) |> InverseDemandFunction
        member x.SymbolicFn = RealFunction(defaultArg x.Symbol "f", x.ScalarVars.[0].Name) |> InverseDemandFunction
        member x.ScalarExpr = x.ScalarExpr
        member x.ScalarVars = _rf.ScalarVars
        member x.Html() = _rf.Html()

type DemandFunction2(f:RealFunction2) =
    inherit RealFunction2(f.ScalarExpr, ?symbol=f.Symbol)
    let _rf = f :> IRealFunction<RealFunction2>
    interface IRealFunction<DemandFunction2> with
        member x.Term = x
        member x.Expr = x.ScalarExpr.Expr
        member x.SymbolicExpr = x.SymbolicFn.ScalarExpr.Expr
        member x.Attrs = x.Attrs
        member x.Symbol = x.Symbol
        member x.Transform(b:Expr<real>, ?attrs, ?s) = _rf.Transform(b, ?a=attrs, ?s=s) |> DemandFunction2
        member x.SymbolicFn = RealFunction2(defaultArg x.Symbol "f", x.ScalarVars.[0].Name, x.ScalarVars.[1].Name) |> DemandFunction2
        member x.ScalarExpr = x.ScalarExpr
        member x.ScalarVars = _rf.ScalarVars
        member x.Html() = _rf.Html()

type SupplyFunction(f:RealFunction) =
    inherit RealFunction(f.ScalarExpr, ?symbol=f.Symbol)
    let _rf = f :> IRealFunction<RealFunction>
    interface IRealFunction<SupplyFunction> with
        member x.Term = x
        member x.Expr = x.ScalarExpr.Expr
        member x.SymbolicExpr = x.SymbolicFn.ScalarExpr.Expr
        member x.Attrs = x.Attrs
        member x.Symbol = x.Symbol
        member x.Transform(b:Expr<real>, ?attrs, ?s) = _rf.Transform(b, ?a=attrs, ?s=s) |> SupplyFunction
        member x.SymbolicFn = RealFunction(defaultArg x.Symbol "f", x.ScalarVars.[0].Name) |> SupplyFunction
        member x.ScalarExpr = x.ScalarExpr
        member x.ScalarVars = _rf.ScalarVars
        member x.Html() = _rf.Html()
  
type SupplyFunction2(f:RealFunction2) =
    inherit RealFunction2(f.ScalarExpr, ?symbol=f.Symbol)
    let _rf = f :> IRealFunction<RealFunction2>
    interface IRealFunction<SupplyFunction2> with
        member x.Term = x
        member x.Expr = x.ScalarExpr.Expr
        member x.SymbolicExpr = x.SymbolicFn.ScalarExpr.Expr
        member x.Attrs = x.Attrs
        member x.Symbol = x.Symbol
        member x.Transform(b:Expr<real>, ?attrs, ?s) = _rf.Transform(b, ?a=attrs, ?s=s) |> SupplyFunction2
        member x.SymbolicFn = RealFunction2(defaultArg x.Symbol "f", x.ScalarVars.[0].Name, x.ScalarVars.[1].Name) |> SupplyFunction2
        member x.ScalarExpr = x.ScalarExpr
        member x.ScalarVars = _rf.ScalarVars
        member x.Html() = _rf.Html()

type ProductionFunction(f:RealFunction) =
    inherit RealFunction(f.ScalarExpr, ?symbol=f.Symbol)
    let _rf = f :> IRealFunction<RealFunction>
    interface IRealFunction<ProductionFunction> with
        member x.Term = x
        member x.Expr = x.ScalarExpr.Expr
        member x.SymbolicExpr = x.SymbolicFn.ScalarExpr.Expr
        member x.Attrs = x.Attrs
        member x.Symbol = x.Symbol
        member x.Transform(b:Expr<real>, ?attrs, ?s) = _rf.Transform(b, ?a=attrs, ?s=s) |> ProductionFunction
         member x.SymbolicFn = RealFunction(defaultArg x.Symbol "f", x.ScalarVars.[0].Name) |> ProductionFunction
        member x.ScalarExpr = x.ScalarExpr
        member x.ScalarVars = _rf.ScalarVars
        member x.Html() = _rf.Html()
        
type ProductionFunction2(f:RealFunction2) =
    inherit RealFunction2(f.ScalarExpr, ?symbol=f.Symbol)
    let _rf = f :> IRealFunction<RealFunction2>
    interface IRealFunction<ProductionFunction2> with
        member x.Term = x
        member x.Expr = x.ScalarExpr.Expr
        member x.SymbolicExpr = x.SymbolicFn.ScalarExpr.Expr
        member x.Attrs = x.Attrs
        member x.Symbol = x.Symbol
        member x.Transform(b:Expr<real>, ?attrs, ?s) = _rf.Transform(b, ?a=attrs, ?s=s) |> ProductionFunction2
        member x.SymbolicFn = RealFunction2(defaultArg x.Symbol "f", x.ScalarVars.[0].Name, x.ScalarVars.[1].Name) |> ProductionFunction2
        member x.ScalarExpr = x.ScalarExpr
        member x.ScalarVars = _rf.ScalarVars
        member x.Html() = _rf.Html()

type CostFunction(f:RealFunction) =
    inherit RealFunction(f.ScalarExpr, ?symbol=f.Symbol)
    let _rf = f :> IRealFunction<RealFunction>
    interface IRealFunction<CostFunction> with
        member x.Term = x
        member x.Expr = x.ScalarExpr.Expr
        member x.SymbolicExpr = x.SymbolicFn.ScalarExpr.Expr
        member x.Attrs = x.Attrs
        member x.Symbol = x.Symbol
        member x.Transform(b:Expr<real>, ?attrs, ?s) = _rf.Transform(b, ?a=attrs, ?s=s) |> CostFunction
        member x.SymbolicFn = RealFunction(defaultArg x.Symbol "f", x.ScalarVars.[0].Name) |> CostFunction
        member x.ScalarExpr = x.ScalarExpr
        member x.ScalarVars = _rf.ScalarVars
        member x.Html() = _rf.Html()
        
type CostFunction2(f:RealFunction2) =
    inherit RealFunction2(f.ScalarExpr, ?symbol=f.Symbol)
    let _rf = f :> IRealFunction<RealFunction2>
    interface IRealFunction<CostFunction2> with
        member x.Term = x
        member x.Expr = x.ScalarExpr.Expr
        member x.SymbolicExpr = x.SymbolicFn.ScalarExpr.Expr
        member x.Attrs = x.Attrs
        member x.Symbol = x.Symbol
        member x.Transform(b:Expr<real>, ?attrs, ?s) = _rf.Transform(b, ?a=attrs, ?s=s) |> CostFunction2
        member x.SymbolicFn = RealFunction2(defaultArg x.Symbol "f", x.ScalarVars.[0].Name, x.ScalarVars.[1].Name) |> CostFunction2
        member x.ScalarExpr = x.ScalarExpr
        member x.ScalarVars = _rf.ScalarVars
        member x.Html() = _rf.Html()

type RevenueFunction(f:RealFunction) =
    inherit RealFunction(f.ScalarExpr, ?symbol=f.Symbol)
    let _rf = f :> IRealFunction<RealFunction>
    interface IRealFunction<RevenueFunction> with
        member x.Term = x
        member x.Expr = x.ScalarExpr.Expr
        member x.SymbolicExpr = x.SymbolicFn.ScalarExpr.Expr
        member x.Attrs = x.Attrs
        member x.Symbol = x.Symbol
        member x.Transform(b:Expr<real>, ?attrs, ?s) = _rf.Transform(b, ?a=attrs, ?s=s) |> RevenueFunction
        member x.SymbolicFn = RealFunction(defaultArg x.Symbol "f", x.ScalarVars.[0].Name) |> RevenueFunction
        member x.ScalarExpr = x.ScalarExpr
        member x.ScalarVars = _rf.ScalarVars
        member x.Html() = _rf.Html()
      
type Gamble(distr:seq<real*real>) =
    do if distr |> Seq.map snd |> Seq.reduce (+) <> 1.0 then failwith "The probabilities in the gamble must add up to 1."
    member val Distr = distr |> Map.ofSeq
    member val Outcomes = distr |> Seq.map (fst >> Scalar<real>) |> Seq.toList
    member val Probabilities = distr |> Seq.map (snd >> Scalar<real>) |> Seq.toList
    member x.Prob(v) = x.Distr.[v] |> Scalar<real>
    member val Expectation = distr |> Seq.map(fun (v, p) -> v * p) |> Seq.reduce (+) |> Scalar<real>

type Tax =
| AdValorem
| Excise
| LumpSum

type EconomicModel() = 
    member val Attrs = new Dictionary<string, obj>()
    member val Vars = new Dictionary<string, realvar>()
    member val Functions = new Dictionary<string, RealFunction>()
    member val Functions2 = new Dictionary<string, RealFunction2>()
    abstract Constraints:ScalarRelation<real> list
    default x.Constraints = List.empty
    member x.Equations = x.Constraints |> List.choose(fun c -> if c :? ScalarEquation<real> then c :?> ScalarEquation<real> |> Some else None) 
    member internal x.CreateVar(name:string, ?nt:string) = x.Vars.[name] <- realvar (defaultArg nt name)    
    member x.CreateVars([<ParamArray>] n:string[]) = n |> Array.iter x.CreateVar
    member x.CreateVars([<ParamArray>] n:(string*string)[]) = n |> Array.iter (fun (v,l) -> x.CreateVar(v, l))
    member x.GetVar n = if x.Vars.ContainsKey n then x.Vars.[n] else failwithf "The model does not contain the real variable %A." n
    member x.SetVar(n, v) = x.Vars.[n] <- v
    member x.GetFun<'a when 'a :> RealFunction> n = x.Functions.[n] :?> 'a
    member x.SetFun<'a when 'a :> RealFunction> (n, f:'a)= 
        let mvars = x.Vars.Values |> Seq.map(fun v -> v.Var) in
        let vars = f.Vars in
        let mv = Seq.tryFind(fun v -> not <| Seq.contains v mvars) vars in
        if mv.IsSome then failwithf "The function %A contains variables %A not in the model" f mv.Value
        x.Functions.[n] <- f
    member x.GetFun2<'a when 'a :> RealFunction2> n = x.Functions2.[n] :?> 'a
    member x.SetFun2<'a when 'a :> RealFunction2> (n, f:'a)= 
        (*
        let mvars = x.Vars.Values |> Seq.map(fun v -> v.Var) in
        let vars = f.Vars in
        let mv = Seq.tryFind(fun v -> not <| Seq.contains v mvars) vars in
        if mv.IsSome then failwithf "The function %A contains variables %A not in the model" f mv.Value
        *)
        x.Functions2.[n] <- f
    member x.CreateFun(name:string, v:realvar) = x.SetFun<RealFunction>(name, realfun_s name v)
    member x.CreateProdFun(name:string, v:realvar) = x.SetFun<ProductionFunction>(name, realfun_s name v |> ProductionFunction)
    member x.CreateUtilFun(name:string, v:realvar) = x.SetFun<UtilityFunction>(name, realfun_s name v |> UtilityFunction)
    member x.CreateUtilFun(name:string, v:string) = x.SetFun<UtilityFunction>(name, realfun_s name (x.GetVar v) |> UtilityFunction)
    member x.CreateProdFun2(name:string, v1:realvar, v2:realvar) = x.SetFun2<ProductionFunction2>(name, realfun2_s name v1 v2 |> ProductionFunction2)
    member x.CreateUtilFun2(name:string, v1:realvar, v2:realvar) = x.SetFun2<UtilityFunction2>(name, realfun2_s name v1 v2 |> UtilityFunction2)
    member x.CreateUtilFun2(name:string, v1:string, v2:string) = x.SetFun2<UtilityFunction2>(name, realfun2_s name (x.GetVar v1) (x.GetVar v2) |> UtilityFunction2)
    interface IAttrs with member x.Attrs = x.Attrs

module Economics =
    let marginal (x:realvar) (func:IRealFunction<'a>)  = 
     match func.Symbol with
     | None -> diff x func |> with_attr_tag "Marginal" 
     | Some s -> diff x func |> with_attr_tag "Marginal" |> with_symbol ("M" + s.JoinSuperscript(x.Name))

    let marginal_e x func  = marginal x func |> fexpr

    let elasticity (x:realvar) (func:IRealFunction<'a>) =
     let d = diffe x func
     let s = d * (x / func.ScalarExpr)
     func.Transform(s.Expr) |> with_attr_tag "Elasticity"

    let elasticity_e x func = elasticity x func |> fexpr
     
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
    
    let demandfun_s (s:string) (v:string) = RealFunction(s, v) |> DemandFunction

    let supplyfun_s (s:string) (v:string) = RealFunction(s, v) |> SupplyFunction

    let utilfun_s (s:string) (v:string) = RealFunction(s, v) |> UtilityFunction
       
    let utilfun2_s (s:string) (v1:string) (v2:string) = RealFunction2(s, v1, v2) |> UtilityFunction2

    let prodfun_s (s:string) (v:string)  = RealFunction(s, v) |> ProductionFunction

    let prodfun2_s s (v1:string) (v2:string) = RealFunction2(s, v1, v2) |> ProductionFunction2
    
    let inv_demandfun (sym:string) (q:realvar) (f:DemandFunction) =
     let p = farg f
     let s = solve_unique {|posvars=true|} p [(q == f.[p])] 
     realfun sym s |> InverseDemandFunction

    let revenuefun (s: string) (f:InverseDemandFunction) =
     let q = farg f
     realfun s (q * f.[q]) |> RevenueFunction

    let budget_constraint (r:ScalarEquation<real>) = r |> with_attr_tag "BudgetConstraint"

    let isoquants (attrs:'a) (dv:realvar) (f:DemandFunction2) (vals:seq<real>) =
     let fs = vals |> Seq.map(fun v -> prodfun_im (sprintf "%s = %A" dv.Name v) dv (f == v) :> IRealFunction<RealFunction>) 
     draw attrs <| realfungrpv fs

    let indifference_curves (attrs:'a) (dv:realvar) (f:UtilityFunction2) (vals:seq<real>) =
     let fs = vals |> Seq.map(fun v -> utilfun_im (sprintf "%s = %A" dv.Name v) dv (f == v) :> IRealFunction<RealFunction>) 
     draw attrs <| realfungrpv fs

    let constrained_indifference_curves (attrs:'a) (dv:realvar) (f:UtilityFunction2) (c:ScalarEquation<real>) (vals:seq<real>) =
     let fs = 
        vals 
        |> Seq.map(fun v -> utilfun_im (sprintf "%s = %A" dv.Name v) dv (f == v)) 
        |> Seq.append([|realfun_im "" dv (c.Lhs == c.Rhs) |> UtilityFunction|]) 
        |> Seq.cast<IRealFunction<RealFunction>>
     draw attrs <| realfungrpv fs

    let mrs (f:IRealFunction<RealFunction2>) =
     let M1 = partdiffn_e 0 f
     let M2 = partdiffn_e 1 f
     (M1 / M2) |> ratsimp

    let price_elasticity_demand (f:DemandFunction) =
        let p = farg f in elasticity p f
    
    let gamble distr = Gamble distr

    let econ_model<'m when 'm :> EconomicModel and 'm: (new : unit -> 'm)>  = new 'm()
    
    let get_model_var (m:EconomicModel) v = m.GetVar v
    
    let set_model_var (m:EconomicModel) n v = m.SetVar(n, v)
    
    let get_model_fun<'f when 'f :> RealFunction> (m:EconomicModel) f :'f = m.GetFun<'f> f
    
    let set_model_fun (m:EconomicModel) n (f:'f when 'f :> RealFunction) = m.SetFun(n, f)
    
    let with_model_fun n f m = set_model_fun m n f ; m
    
    let get_model_fun2<'f when 'f :> RealFunction2> (m:EconomicModel) f :'f = m.Functions2.[f] :?> 'f

    let set_model_fun2 (m:EconomicModel) n (f:'f when 'f :> RealFunction2) = m.SetFun2(n, f)

    let with_model_fun2 n f m = set_model_fun2 m n f ; m

    let solve_model (options:'a) (m:EconomicModel) =
        let vars = m.Equations |> List.collect (fun e -> e.ScalarVars)
        solve options vars m.Equations
    
    let solve_model_for (x:realvar list) (m:EconomicModel) = solve {|posvars=true|} x m.Equations