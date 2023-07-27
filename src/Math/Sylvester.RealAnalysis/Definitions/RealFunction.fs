namespace Sylvester

open FSharp.Quotations

type RealFunction<'t when 't : equality>(domain:ISet<'t>, codomain:ISet<real>, map:Expr<'t->real>) = 
    inherit ScalarFunction<'t, real>(domain, codomain, map)
    member x.Arg = Scalar<real> (expand_as<real> x.Vars.[0])
    
type RealFunction(f) = 
    inherit RealFunction<real>(Field.R, Field.R, f)
    new (e:Scalar<real>) =
        let v = get_vars e.Expr
        do if v.Length <> 1 then failwith "The number of independent variables in this function is not 1."
        let f = recombine_func_as<real->real> v e.Expr in
        RealFunction f
 
type SetFunction<'t when 't: equality>(domain:Set<Set<'t>>, codomain:Set<real>, map:MapExpr<Set<'t>, real>) = inherit RealFunction<Set<'t>>(domain, codomain, map)

[<AutoOpen>]
module RealFunction =
    let realfun (e:Scalar<real>) = RealFunction e