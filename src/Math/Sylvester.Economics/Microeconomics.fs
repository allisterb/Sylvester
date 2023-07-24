namespace Sylvester

open FSharp.Quotations

type EconomicFunction(e:Scalar<real>) = 
    inherit RealFunction(e)
    interface ISymbolic<EconomicFunction, real> with
        member a.Expr = a.Body
        member a.Mutate(b:Expr<real>) = EconomicFunction <| Scalar<real> b 
            
[<AutoOpen>]
module MicroEconomics =
    let marginal (func:EconomicFunction) :EconomicFunction = diff func.Arg func
    
    let demand func = EconomicFunction func

    let supply func = EconomicFunction func

    let cost func = EconomicFunction func

    let revenue func = EconomicFunction func

    let plot_name (func:EconomicFunction) = Expr.Value((sprintf "q_D(%A) = %s" (farg func) (sprinte <| fexpr func)) :> obj)