namespace Sylvester

open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns
 
type MarketEquilibrium() =
    inherit EconomicModel()
    do
        base.Vars.["p"] <- realvar "p"
        base.Vars.["q"] <- realvar "q"
        base.Vars.["T"] <- realvar "T"

    member x.p
        with get() = x.GetVar "p"
        and set(value) = x.SetVar ("p", value)
    member x.q
        with get() = x.GetVar "q"
        and set(value) = x.SetVar ("q", value)
    member x.Qs
        with get() = x.GetFun<SupplyFunction> "Qs"
        and set(value:SupplyFunction) = x.SetFun("Qs", value)
    member x.Qd
           with get() = x.GetFun<DemandFunction> "Qd"
           and set(value:DemandFunction) = x.SetFun("Qd", value)
    member x.MarketEquilibrium = EqualityConstraint (x.Qs.[x.p] == x.Qd.[x.p])