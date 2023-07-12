namespace Sylvester

open FSharp.Quotations

type EconomicFunction(map:MapExpr<real, real>) = 
    inherit RealFunction(map)
    interface ISymbolic<EconomicFunction, real> with
        member a.Expr = a.Body
        member a.Mutate(b:Expr<real>) = 
            let mmap = expand_as<real->real> (recombine_func (get_vars a.Body) b) in EconomicFunction mmap 
            
[<AutoOpen>]
module MicroEconomics =
    let marginal (func:EconomicFunction) :EconomicFunction = diff func.Arg func
    
    let demand func = EconomicFunction func

    let supply func = EconomicFunction func

    let cost func = EconomicFunction func

    let revenue func = EconomicFunction func