namespace Sylvester

type FirmProfit() =
    inherit EconomicModel()
    do
        base.Vars.["K"] <- realvar "K"
        base.Vars.["Nd"] <- realvar "N_d"
        base.Vars.["z"] <- realvar "z"
        base.Vars.["w"] <- realvar "w"
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
    member x.Y = x.z * x.F.[x.K, x.Nd]
    member x.Pi = x.Y - x.w * x.Nd 


