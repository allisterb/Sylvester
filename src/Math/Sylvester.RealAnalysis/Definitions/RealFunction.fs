namespace Sylvester

open Vector
open Dimension

open System.Reflection
open FSharp.Quotations

type RealFunction<'t when 't : equality>(domain:ISet<'t>, codomain:ISet<real>, map:Expr<'t->real>) = 
    inherit ScalarFunction<'t, real>(domain, codomain, map)
    member x.Arg = Scalar<real> (expand_as<real> x.Vars.[0])
    
type RealFunction<'t, 'a when 't : equality and 'a: equality>(domain:ISet<'t>, codomain:ISet<real>, map:Expr<'t->real>, amap:Expr<'a->'t>) = 
    inherit ScalarFunction<'t, real, 'a>(domain, codomain, map, amap)
    member x.Arg = Scalar<real> (expand_as<real> x.Vars.[0])

type RealFunction(f) = 
    inherit RealFunction<real>(Field.R, Field.R, f)
    new (e:Scalar<real>) =
        let v = get_vars e.Expr
        do if v.Length <> 1 then failwith "The number of independent variables in this function is not 1."
        let f = recombine_func_as<real->real> v e.Expr in
        RealFunction f
 
 type RealFunction2(f:Expr<Vector<dim<2>, real>->real>) = 
     inherit RealFunction<Vec<dim<2>>, real*real>(R<dim<2>>(), Field.R, f, <@ fun (x, y) -> vec2 x y @>)
     new (e:Scalar<real>) =
         let vars = get_vars e.Expr
         let vvec = Var("v", typeof<Vec<dim<2>>>)
         let vv = vvec |> Expr.Var |> expand_as<Vec<dim<2>>>

         do if vars.Length <> 2 then failwith "The number of independent variables in this function is not 2."
         let mutable me = e.Expr.Raw
         let c = Expr.Call(vv, typeof<Vec<dim<2>>>.GetMethod("get_ItemE"), (<@@ 0 @@>)::[])
         vars |> List.iteri(fun i v -> me <- subst_var_value v (Expr.Call(vv, typeof<Vec<dim<2>>>.GetMethod("get_ItemE"), ((exprv i).Raw)::[])) e.Expr )
         let f = recombine_func_as<Vec<dim<2>>->real> [vvec] me in
         RealFunction2 f

type SetFunction<'t when 't: equality>(domain:Set<Set<'t>>, codomain:Set<real>, map:MapExpr<Set<'t>, real>) = inherit RealFunction<Set<'t>>(domain, codomain, map)

[<AutoOpen>]
module RealFunction =
    let realfun (e:Scalar<real>) = RealFunction e