namespace Sylvester

open Vector
open Dimension

open System.Reflection
open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns

type RealFunction<'t when 't : equality>(domain:ISet<'t>, codomain:ISet<real>, map:Expr<'t->real>) = 
    inherit ScalarFunction<'t, real>(domain, codomain, map)
    
type RealFunction<'t, 'a when 't : equality and 'a: equality>(domain:ISet<'t>, codomain:ISet<real>, map:Expr<'t->real>, amap:Expr<'a->'t>) = 
    inherit ScalarFunction<'t, real, 'a>(domain, codomain, map, amap)
   
type RealFunction(f) = 
    inherit RealFunction<real>(Field.R, Field.R, f)
    new (e:Scalar<real>) =
        let v = get_vars e.Expr
        do if v.Length <> 1 then failwith "The number of independent variables in this function is not 1."
        let f = recombine_func_as<real->real> v e.Expr in
        RealFunction f
    //interface IWebVisualization with
    //    member x.Draw(attrs:obj) = 
    //    ()
 
 type RealFunction2(f:Expr<Vector<dim<2>, real>->real>, ?af:Expr<real*real->Vec<dim<2>>>, ?sf:Expr<(real*real)->real>) = 
     inherit RealFunction<Vec<dim<2>>, real*real>(R ``2``, Field.R, f, defaultArg af <@ fun (x, y) -> vec2 x y @>)
     member val ScalarMapExpr = 
        match sf with
        | Some f -> f
        | None ->
            let vars = Var("x", typeof<real>)::Var("y", typeof<real>)::[]  in
            let vv = base.Arg |> Expr.Var |> expand_as<Vec<dim<2>>>
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
            let vv = x.Arg |> Expr.Var |> expand_as<Vec<dim<2>>>
            let m = typeof<Vec<dim<2>>>.GetProperty("ItemE")
            vars |> List.iteri(fun i v -> me <- replace_expr (Expr.PropertyGet(vv, m, ((exprv i).Raw)::[])) v me )
            me |> expand_as<real>
        | _ -> failwithf "%s is not a valid expression for argument substitution." (sprinte v)
     member x.Item(a:_, b:_) = 
        let _a, _b = realexpr a, realexpr b in
        x.SubstArg <@ %_a, %_b @> |> Scalar<real>
     new (e:Scalar<real>) =
         let vars = e |> sexpr |> get_vars |> List.sortBy(fun v -> v.Name)
         do if vars.Length <> 2 then failwith "The number of independent variables in this function is not 2."
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
         RealFunction2 (f, af, sf)

type SetFunction<'t when 't: equality>(domain:Set<Set<'t>>, codomain:Set<real>, map:MapExpr<Set<'t>, real>) = inherit RealFunction<Set<'t>>(domain, codomain, map)

type RealFunctionDiagram = {
    xmin:float
    xmax:float
}
[<AutoOpen>]
module RealFunction =
    let realfun (e:Scalar<real>) = RealFunction e

    let realfun2 (e:Scalar<real>) = RealFunction2 e

    let rec make_JS_compat    = 
        function
        | SpecificCall <@@ ( ** ) @@> (_, _, [xt; yt])  -> 
            let xxt, yyt = make_JS_compat xt, make_JS_compat yt
            <@@ FunScript.Arithmetic.MathJS.Pow((%%xxt:float), (%%yyt:float)) @@>
        | expr -> traverse expr make_JS_compat