namespace Sylvester

open System
open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns
[<AbstractClass>]
type RealFunction<'t, 'a when 't : equality and 'a: equality>(domain:ISet<'t>, codomain:ISet<real>, map:Expr<'t->real>, amap:Expr<'a->'t>, ?symbol:string) = 
    inherit ScalarFunction<'t, real, 'a>(domain, codomain, map, amap, ?symbol=symbol)

    abstract ScalarExpr:Scalar<real>
   
    abstract ScalarVars: realvar list

    member x.Expr = x.ScalarExpr.Expr

    static member (==) (l:RealFunction<'t, 'a>, r:RealFunction<'t, 'a>) = l.ScalarExpr == r.ScalarExpr 

    static member (==) (l:RealFunction<'t, 'a>, r:Scalar<real>) = l.ScalarExpr == r
       
    static member (==) (l:RealFunction<'t, 'a>, r:real) = l.ScalarExpr == r

    static member (+) (l:RealFunction<'t, 'a>, r:RealFunction<'t, 'a>) = l.ScalarExpr + r.ScalarExpr 

type IRealFunction<'a> = 
    inherit ISymbolic<'a, real>
    inherit ISymbolicExpr<'a, real>
    inherit IHtmlDisplay
    inherit IWebVisualization
    abstract member ScalarVars:realvar list
    abstract member ScalarExpr:Scalar<real>
    abstract member SymbolicFn:'a

[<StructuredFormatDisplay("{UnicodeDisplay}")>]
type RealFunction(n:int, f, s, ?vars: ScalarVar<real> list, ?symbol:string) = 
    inherit RealFunction<Vector<real>, real[]>(R(n), Field.R, f, <@ Vector<real> @>, ?symbol=symbol)
    let sv =  defaultArg vars (get_real_vars s)
    member private x.substarg (args : Expr list) =
        if args.Length <> n then failwith "The number of function arguments supplied is not equal to the dimension of the function."
        let mutable me = x.Body.Raw
        let vv = x.ArgExpr
        let m = typeof<Vec>.GetProperty("ItemE")
        args |> List.iteri(fun i v -> me <- replace_expr (Expr.PropertyGet(vv, m, ((exprv i).Raw)::[])) v me )
        me |> expand_as<real> |> simplifye
    member private x.substarg (args : Expr<real> list) = args |> List.map (fun e -> e.Raw) |> x.substarg
    member val Dim = n
    override val ScalarExpr = s
    override val ScalarVars = sv
    override x.SubstArg(v:Expr) = 
           match v with
           | NewTuple(args) ->
               x.substarg args
           | NewArray(t, args) when t = typeof<real> ->
               x.substarg args 
           | _ -> failwithf "%s is not a valid expression for argument substitution." (sprinte v)
    
    member x.Item([<ParamArray>] args:real array) =
        args |> Array.toList |> List.map realexpr |> x.substarg |> ev

    member x.Item([<ParamArray>] args:obj array) =
        args |> Array.toList |> List.map realexpr |> x.substarg |> Scalar

    member x.SymbolicFn =
        let eva = Expr.NewArray(typeof<string>, sv |> List.map (var_name >> exprv >> fun v -> v.Raw) )
        let symbol = defaultArg symbol "f"
        let syv = exprv symbol
        let nb = <@ symbolic_fn<real> (%syv) (%%eva:string[]) @>
        let vv = new Var("v", typeof<Vec>)
        
        let nf = recombine_func_as<Vector<real>->real> [vv] nb in
        RealFunction(Scalar nb, sv, symbol)

    member x.IsSymbolic : bool = 
        match s.Expr with
        |  SpecificCall <@@ symbolic_fn @@> (_,_,[String _;NewArray(_, _)]) -> true
        | _ -> false

    member val UnicodeDisplay =
        if s |> sexpr |> is_symbolic_fn  then s |> sprints  else
            let v = if sv.Length > 0 then sv |> List.map sprints |> List.reduce (sprintf "%s,%s") else ""  
            match symbol with
            | None -> sprints s 
            | Some sy -> sprintf "%s(%s) = %s" sy v (sprints s)

    member x.Html() = 
        let v = if x.ScalarVars.Length > 0 then x.ScalarVars |> List.map latex |> List.reduce (sprintf "%s,%s") else "" 
        match x.Symbol with
        | None -> "$" + latex x.ScalarExpr + "$"
        | Some s -> sprintf "$%s(%s) = %s$" s v (latex x.ScalarExpr)

    member x.JSDrawFunc() =
        if x.Dim > 1 then failwith ""
        let v = get_var x.Expr
        recombine_func_as<real->real> [v] x.Expr
        
    static member (+) (l:RealFunction, r:Scalar<real>) = call_add (l.Expr) (r.Expr) |> expand_as<real> |> Scalar |> RealFunction
            
    static member (+) (l:Scalar<real>, r:RealFunction) = call_add (l.Expr) (r.Expr) |> expand_as<real> |> Scalar |> RealFunction
       
    static member (-) (l:RealFunction, r:Scalar<real>) = call_sub (l.Expr) (r.Expr) |> expand_as<real> |> Scalar |> RealFunction
            
    static member (-) (l:Scalar<real>, r:RealFunction) = call_sub (l.Expr) (r.Expr) |> expand_as<real> |> Scalar |> RealFunction

    static member (*) (l:RealFunction, r:Scalar<real>) = call_mul (l.Expr) (r.Expr) |> expand_as<real> |> Scalar |> RealFunction
            
    static member (*) (l:Scalar<real>, r:RealFunction) = call_mul (l.Expr) (r.Expr) |> expand_as<real> |> Scalar |> RealFunction

    static member (/) (l:RealFunction, r:Scalar<real>) = call_div (l.Expr) (r.Expr) |> expand_as<real> |> Scalar |> RealFunction
         
    static member (/) (l:Scalar<real>, r:RealFunction) = call_div (l.Expr) (r.Expr) |> expand_as<real> |> Scalar |> RealFunction

    static member ( ***) (l:RealFunction, r:Scalar<real>) = call_pow (l.Body) (r.Expr) |> expand_as<real> |> Scalar |> RealFunction
            
    static member ( ***) (l:Scalar<real>, r:RealFunction) = call_pow (l.Expr) (r.Expr) |> expand_as<real> |> Scalar |> RealFunction

    interface IRealFunction<RealFunction> with
       member x.Term = x
       member x.Expr = x.ScalarExpr.Expr
       member x.SymbolicExpr = x.SymbolicFn.ScalarExpr.Expr
       member x.Attrs = x.Attrs
       member x.Symbol = x.Symbol
       member a.Transform(b:Expr<real>, ?attrs, ?s) = 
           let f = RealFunction(Scalar<real> b, ?symbol=s)
           do f.Attrs.AddAll(defaultArg attrs null) |> ignore
           f
       member a.ScalarVars = a.ScalarVars 
       member a.ScalarExpr = a.ScalarExpr
       member a.SymbolicFn = a.SymbolicFn
       member a.Html() = a.Html()
       member x.Draw(attrs:_) = 
           
           WebVisualization.draw_realfun2 attrs ((x :> IRealFunction<RealFunction>).Html()) (x.JSDrawFunc()) |> draw_board

    new (f:Scalar<real>, ?realvars: ScalarVar<real> list, ?symbol:string) =
        let vars = get_vars f.Expr
        let m = typeof<Vec>.GetProperty("ItemE") in
        let mutable me = f.Expr.Raw
        let vv = new Var("v", typeof<Vec>)
        do vars |> List.map Expr.Var |> List.iteri(fun i v -> me <- replace_expr v (Expr.PropertyGet(Expr.Var vv, m, ((exprv i).Raw)::[])) me )
        let nb = expand_as<real> me
        let nf = recombine_func_as<Vector<real>->real> [vv] nb in
        RealFunction(vars.Length, nf, f, ?vars=realvars, ?symbol=symbol)

    new (f:Expr<real->real>) = let s = f |> body' |> Scalar in RealFunction s

    new (symbol:string, [<ParamArray>] x:string array) =
        let eva = Expr.NewArray(typeof<string>, x |> Array.toList |> List.map (exprv >> fun v -> v.Raw))
        let vars = x |> Array.toList |> List.map realvar
        let sv = exprv symbol
        let vvv = Scalar<real>  <@ symbolic_fn<real> (%sv) (%%eva:string[]) @>
        RealFunction(vvv, vars, symbol)

    new (eqn:ScalarVarMap<real>) = RealFunction(eqn.Rhs, symbol=eqn.Var.Name)
    
 type RealFunctionGroupVisualization(_grp:seq<IRealFunction<RealFunction>>) =
    interface IWebVisualization with
           member x.Draw(attrs:_) = 
              //let grp = Seq.toArray _grp in
              WebVisualization.draw_realfuns attrs (_grp |> Seq.map(fun x->(x :> IRealFunction<_>).Html()) |> Seq.toArray) (_grp |> Seq.map(fun x ->x.Term.JSDrawFunc()) |> Seq.toArray) |> draw_board

[<AbstractClass>]
type SetFunction<'t when 't: equality>(domain:Set<Set<'t>>, codomain:Set<real>, map:MapExpr<Set<'t>, real>, ?symbol:string) = 
    inherit RealFunction<Set<'t>, Set<'t>>(domain, codomain, map, <@ id @>, ?symbol=symbol)

[<AutoOpen>]
module RealFunction =
    let realfun (s:string) (e:Scalar<real>) = RealFunction(e, symbol=s)

    let realfun_s (s:string) (x:realvar seq) = RealFunction(s, x |> Seq.map var_name |> Seq.toArray)

    let realfun_l (l:Expr<real->real>) = RealFunction(l)

    let realfun_of (s:string) (v:realvar seq) (e:Scalar<real>) =
        let cn = e |> get_real_vars |> List.except v |> List.map var_name in
        let vn = v |> Seq.map var_name in
        let fe = e |> fixconst cn |> const_to_var vn in
        realfun s fe

    let realfun_im (s:string) (x:realvar) (e:ScalarEquation<real>) = let l = Ops.SolveFor x.Expr [e.Expr] in realfun s (scalar_varmap<real> l.Head).Rhs 

    let realfun_im_pos_vars (s:string) (x:realvar) (e:ScalarEquation<real>) = 
        let l = Ops.SolveForPosVars x.Expr [e.Expr] in 
        if l.Length = 1 then realfun s (scalar_varmap<real> l.[0]).Rhs else failwithf "More than one solution was returned for %A. Cannot create a function with this as the dependent variable." x 

    //let realfun2_s (s:string) (x:ScalarVar<real>) (y:ScalarVar<real>)= RealFunction2(s, x.Name, y.Name)

    let realfungrpv g = RealFunctionGroupVisualization g

    let fvar n (f:IRealFunction<_>) = f.ScalarVars.[n]

    let partdiffn_e n (f:IRealFunction<_>) = diffe (fvar n f) f

    let solve_for_fun (y:realvar) (x:realvar) (eqns: ScalarEquation<real> list) =
        let vars = eqns |> List.map sexpr |> get_varsl |> List.map vname
        let consts = List.except [x.Var.Name] vars
        solve_for_single y [x] eqns |> collect_terms x |> fixconst consts |> realfun y.Name

    let solve_for_fun_elim (y:realvar) (e:realvar list) (x:realvar) (eqns: ScalarEquation<real> list) =
           let vars = eqns |> List.map sexpr |> get_varsl |> List.map vname
           let consts = List.except [x.Var.Name] vars 
           solve_for_elim_single y e eqns |> collect_terms x |> fixconst consts |> realfun y.Name 

    let integrate_fun (f:RealFunction) = integrate f.ScalarVars.[0] f

    let integrate_fun_over a b (f:RealFunction) = integrate_over f.ScalarVars.[0]  a b f

    let integrate_fun_over_R (f:RealFunction) = integrate_over_R f.ScalarVars.[0]  f

