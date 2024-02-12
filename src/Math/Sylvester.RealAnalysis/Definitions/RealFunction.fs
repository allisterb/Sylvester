namespace Sylvester

open Vector
open Dimension

open System.Reflection
open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns

type RealFunction<'t, 'a when 't : equality and 'a: equality>(domain:ISet<'t>, codomain:ISet<real>, map:Expr<'t->real>, amap:Expr<'a->'t>, ?symbol:string) = 
    inherit ScalarFunction<'t, real, 'a>(domain, codomain, map, amap, ?symbol=symbol)
     
type RealFunction<'t when 't : equality>(domain:ISet<'t>, codomain:ISet<real>, map:Expr<'t->real>, ?symbol:string) = 
    inherit ScalarFunction<'t, real>(domain, codomain, map, ?symbol=symbol)
    
type IRealFunction<'a> = 
    inherit ISymbolic<'a, real>
    inherit IHtmlDisplay
    //inherit IWebVisualization
    abstract member ScalarVars:realvar list
    abstract member ScalarExpr:Scalar<real>
   
type RealFunction(f, ?symbol:string) = 
    inherit RealFunction<real>(Field.R, Field.R, f, ?symbol=symbol)
    new (e:Scalar<real>, ?symbol:string) =
        let v = get_vars e.Expr
        
        do if v.Length > 1 then failwith "The number of independent variables in this function is > 1."
        let f = recombine_func_as<real->real> (if v.Length = 0 then [Var("_", typeof<real>)] else v) e.Expr in
        RealFunction(f, ?symbol=symbol)

    interface IRealFunction<RealFunction> with
        member x.Term = x
        member x.Expr = x.Body
        member x.Attrs = x.Attrs
        member x.Symbol = x.Symbol
        member a.Transform(b:Expr<real>, ?attrs, ?s) = 
            let f = RealFunction(Scalar<real> b, ?symbol=s)
            do f.Attrs.AddAll(defaultArg attrs null) |> ignore
            f
        member x.Html() = 
            let v = x.Vars.[0] |> exprvar<real> |> latexe
            match x.Symbol with
            | None -> "$$" + latexe x.Body + "$$"
            | Some s ->  "$$" + (sprintf "%s(%s) = %s" s v (latexe x.Body)) + "$$"
        member a.ScalarVars = a.Vars |> List.map (exprvar >> realvar)
        member a.ScalarExpr = Scalar<real> a.Body
        
    interface IWebVisualization with
        member x.Draw(attrs:_) = 
            WebVisualization.draw_realfun2 attrs ((x :> IRealFunction<RealFunction>).Html()) x.MapExpr |> draw_board
    
    static member (==) (l:RealFunction, r:RealFunction) = ScalarEquation<real>(Scalar<real> l.Body, Scalar<real> r.Body) 

    static member (==) (l:RealFunction, r:Scalar<real>) = ScalarEquation<real>(Scalar<real> l.Body, r)
    
    static member (==) (l:RealFunction, r:real) = ScalarEquation<real>(Scalar<real> l.Body, Scalar<real>(exprv r))

    static member (+) (l:RealFunction, r:RealFunction) = Scalar<real> <@ RealFunction.Default(defaultArg l.Symbol "", l.Vars) + RealFunction.Default(defaultArg r.Symbol "", r.Vars) @>
 
 type RealFunctionGroupVisualization(_grp:seq<IRealFunction<RealFunction>>) =
    interface IWebVisualization with
           member x.Draw(attrs:_) = 
              //let grp = Seq.toArray _grp in
              WebVisualization.draw_realfuns attrs (_grp |> Seq.map(fun x->(x :> IRealFunction<_>).Html()) |> Seq.toArray) (_grp |> Seq.map(fun x ->x.Term.MapExpr) |> Seq.toArray) |> draw_board

 type RealFunction2(f:Expr<Vector<dim<2>, real>->real>, ?af:Expr<real*real->Vec<dim<2>>>, ?sf:Expr<(real*real)->real>, ?s:Expr<real>, ?symbol:string) = 
     inherit RealFunction<Vec<dim<2>>, real*real>(R ``2``, Field.R, f, defaultArg af <@ fun (x, y) -> vec2 x y @>, ?symbol=symbol)
     member val ScalarExpr = 
        match s with
        | Some e -> e
        | None ->
            let vars = Var("x", typeof<real>)::Var("y", typeof<real>)::[]  in
            let vv = base.ArgExpr
            let m = typeof<Vec<dim<2>>>.GetProperty("ItemE") in
            let mutable me = base.Body.Raw
            do vars |> List.map Expr.Var |> List.iteri(fun i v -> me <- replace_expr (Expr.PropertyGet(vv, m, ((exprv i).Raw)::[])) v me )
            let nb = expand_as<real> me
            nb
     member x.ScalarVars = get_vars x.ScalarExpr |> List.map (exprvar >> realvar)
     member val ScalarMapExpr = 
        match sf with
        | Some f -> f
        | None ->
            let vars = Var("x", typeof<real>)::Var("y", typeof<real>)::[]  in
            let vv = base.ArgExpr
            let m = typeof<Vec<dim<2>>>.GetProperty("ItemE") in
            let mutable me = base.Body.Raw
            do vars |> List.map Expr.Var |> List.iteri(fun i v -> me <- replace_expr (Expr.PropertyGet(vv, m, ((exprv i).Raw)::[])) v me )
            let nb = expand_as<real> me
            let tupledArg = Var("tupledArg", typeof<real*real>)
            Expr.Lambda(tupledArg, Expr.Let(vars.[0], Expr.TupleGet(Expr.Var(tupledArg), 0), Expr.Let(vars.[1], Expr.TupleGet(Expr.Var(tupledArg), 1), nb)))
                |> expand_as<(real*real)->real>

     override x.SubstArg(v:Expr) = 
        match v with
        | NewTuple(a::b::[] as vars) ->
            let mutable me = x.Body.Raw
            let vv = x.ArgExpr
            let m = typeof<Vec<dim<2>>>.GetProperty("ItemE")
            vars |> List.iteri(fun i v -> me <- replace_expr (Expr.PropertyGet(vv, m, ((exprv i).Raw)::[])) v me )
            me |> expand_as<real>
        | _ -> failwithf "%s is not a valid expression for argument substitution." (sprinte v)
     member x.Item(a:_, b:_) = 
        let _a, _b = realexpr a, realexpr b in
        x.SubstArg <@ %_a, %_b @> |> Scalar<real>
     new (e:Scalar<real>, ?symbol:string) =
         let vars = e |> sexpr |> get_vars
         do if vars.Length > 2 then failwith "The number of independent variables in this function is more than 2."
         let m = typeof<Vec<dim<2>>>.GetMethod("create")
         let tupledArg = Var("tupledArg", typeof<real*real>) 
         let af = Expr.Lambda(tupledArg, Expr.Let(vars.[0], Expr.TupleGet(Expr.Var(tupledArg), 0), Expr.Let(vars.[1], Expr.TupleGet(Expr.Var(tupledArg), 1), Expr.Call(m, Expr.NewArray(typeof<real>, vars |> List.map Expr.Var)::[]))))
                    |> expand_as<real*real->Vec<dim<2>>>
         let sf = Expr.Lambda(tupledArg, Expr.Let(vars.[0], Expr.TupleGet(Expr.Var(tupledArg), 0), Expr.Let(vars.[1], Expr.TupleGet(Expr.Var(tupledArg), 1), e.Expr)))
                            |> expand_as<(real*real)->real>
         let vvec = Var("v", typeof<Vec<dim<2>>>)
         let vv = vvec |> Expr.Var |> expand_as<Vec<dim<2>>>
         let mutable me = e.Expr.Raw
         let m = typeof<Vec<dim<2>>>.GetProperty("ItemE")
         vars |> List.iteri(fun i v -> me <- subst_var_value v (Expr.PropertyGet(vv, m, ((exprv i).Raw)::[])) me )
         let f = recombine_func_as<Vec<dim<2>>->real> [vvec] me in
         RealFunction2 (f, af, sf, e.Expr, ?symbol=symbol)

     interface IRealFunction<RealFunction2> with
        member x.Term = x
        member x.Expr = x.ScalarExpr
        member x.Attrs = x.Attrs
        member x.Symbol = x.Symbol
        member a.Transform(b:Expr<real>, ?attrs, ?s) = 
            let f = RealFunction2(Scalar<real> b, ?symbol=s)
            do f.Attrs.AddAll(defaultArg attrs null) |> ignore
            f
        member a.ScalarVars = a.ScalarVars 
        member a.ScalarExpr = Scalar<real> a.ScalarExpr
        member x.Html() = 
            let v = x.ScalarVars |> List.skip 1 |> List.fold (fun p n -> sprintf "%s,%s" p (latexe n.Expr)) (x.ScalarVars |> List.head |> sexpr |> latexe)
            match x.Symbol with
            | None -> "$$" + latexe x.ScalarExpr + "$$"
            | Some s ->  "$$" + (sprintf "%s(%s) = %s" s v (latexe x.ScalarExpr)) + "$$"
        //member x.Item(o:obj) =

     static member (==) (l:RealFunction2, r:RealFunction2) = ScalarEquation<real>(Scalar<real> l.ScalarExpr, Scalar<real> r.Body)
     
     static member (==) (l:RealFunction2, r:Scalar<real>) = ScalarEquation<real>(Scalar<real> l.ScalarExpr, r)
     
     static member (==) (l:RealFunction2, r:real) = ScalarEquation<real>(Scalar<real> l.ScalarExpr, Scalar<real>(exprv r))

type SetFunction<'t when 't: equality>(domain:Set<Set<'t>>, codomain:Set<real>, map:MapExpr<Set<'t>, real>, ?symbol:string) = inherit RealFunction<Set<'t>>(domain, codomain, map,?symbol=symbol)

[<AutoOpen>]
module RealFunction =
    let realfun s (e:Scalar<real>) = RealFunction(e, s)

    let realfun2 (s:string) (e:Scalar<real>) = RealFunction2(e, s)

    let realfun_im (s:string) (x:realvar) (e:ScalarEquation<real>) = let l = Ops.SolveFor x.Expr [e.Expr] in realfun s (Scalar<real> l) 

    let realfun_im_pos_vars (s:string) (x:realvar) (e:ScalarEquation<real>) = 
        let l = Ops.SolveForPosVars x.Expr [e.Expr] in 
        if l.Length = 1 then realfun s (Scalar<real> l.[0]) else failwithf "More than one solution was returned for %A. Cannot create a function with this as the dependent variable." x 

    let realfungrpv g = RealFunctionGroupVisualization g

    let fvar n (f:IRealFunction<_>) = f.ScalarVars.[n]

    let partdiffn_e n (f:IRealFunction<_>) = diffe (fvar n f) f