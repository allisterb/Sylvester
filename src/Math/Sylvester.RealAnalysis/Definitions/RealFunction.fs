namespace Sylvester

open FSharp.Quotations

type RealFunction<'t when 't : equality>(domain:ISet<'t>, codomain:ISet<real>, map:Expr<'t->real>) = 
    inherit ScalarFunction<'t, real>(domain, codomain, map)
    
type RealFunction(f) = 
    inherit RealFunction<real>(Field.R, Field.R, f)
    member x.Arg = Scalar<real> (expand_as<real> x.Vars.[0])
 
type SetFunction<'t when 't: equality>(domain:Set<Set<'t>>, codomain:Set<real>, map:MapExpr<Set<'t>, real>) = inherit RealFunction<Set<'t>>(domain, codomain, map)

[<AutoOpen>]
module RealFunction =
    let realfun f = RealFunction f