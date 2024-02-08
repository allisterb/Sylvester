namespace Sylvester

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
    member x.T
        with get() = x.GetVar "T"
        and set(value) = x.SetVar ("T", value)
    member x.Qs
        with get() = x.GetFun<SupplyFunction> "Qs"
        and set(value:SupplyFunction) = x.SetFun("Qs", value)
    member x.Qd
           with get() = x.GetFun<DemandFunction> "Qd"
           and set(value:DemandFunction) = x.SetFun("Qd", value)
    member val Tax = NoTax with get,set
    member x.MarketEquilibrium = EqualityConstraint (if x.Tax = NoTax then x.Qs.[x.p] == x.Qd.[x.p] else x.Qs.[x.p - x.T] == x.Qd.[x.p])