namespace Sylvester

open Economics

type FirmProfit() =
    inherit EconomicModel()
    do
        base.Vars.["K"] <- realvar "K"
        base.Vars.["Nd"] <- realvar "N_d"
        base.Vars.["z"] <- realvar "z"
        base.Vars.["w"] <- realvar "w"
        base.Vars.["Y"] <- realvar "Y"
        base.Vars.["Pi"] <- realvar "Pi"
    member x.K
           with get() = x.GetVar "K"
           and set(value) = x.SetVar ("K", value)
    member x.Nd
        with get() = x.GetVar "Nd"
        and set(value) = x.SetVar ("Nd", value)
    member x.z
        with get() = x.GetVar "z"
        and set(value) = x.SetVar ("z", value)
    member x.w
        with get() = x.GetVar "w"
        and set(value) = x.SetVar ("w", value)
    member x.F
        with get() = x.GetFun2<ProductionFunction2> "F"
        and set(value:ProductionFunction2) = x.SetFun2("F", value)
    member x.Y
        with get() = x.GetVar "Y"
        and set(value) = x.SetVar ("Y", value)
    member x.Pi
           with get() = x.GetVar "Pi"
           and set(value) = x.SetVar ("Pi", value)
    member x.Output = x.Y == x.z * x.F.[x.K, x.Nd]
    member x.Profit = x.Pi == x.Y - x.w * x.Nd 
    member x.InadaConditions = [
        marginal_e x.K x.F +> 0.
        marginal_e x.Nd x.F +> 0.
    ]
    override x.Constraints = [x.Output; x.Profit] @ x.InadaConditions


