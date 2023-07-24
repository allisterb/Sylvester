namespace Sylvester

open FSharp.Quotations

type EconomicFunction(a:ScalarAssignment<real>) = 
    inherit RealFunction(a.Body)
    member val Assignment = a
    interface ISymbolic<EconomicFunction, real> with
        member a.Expr = a.Body
        member a.Mutate(b:Expr<real>) = EconomicFunction <| ScalarAssignment<real>(a.Assignment.Arg, Scalar<real> b) 
            
[<AutoOpen>]
module MicroEconomics =
    let marginal (func:EconomicFunction) :EconomicFunction = diff func.Arg func
    
    let demand func = EconomicFunction func

    let supply func = EconomicFunction func

    let cost func = EconomicFunction func

    let revenue func = EconomicFunction func

    let plot_name (func:EconomicFunction) = Expr.Value((sprintf "q_D(%A) = %s" (farg func) (sprinte <| fexpr func)) :> obj)