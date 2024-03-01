namespace Sylvester

type Insurance() =
    inherit EconomicModel()
    do
        base.Vars.["c1"] <- realvar "c1"
        base.Vars.["c2"] <- realvar "c2"
        base.Vars.["p1"] <- realvar "p1"
        base.Vars.["p2"] <- realvar "p2"
        base.Vars.["U"] <- realvar "U"
        base.Vars.["K"] <- realvar "K"
    member x.c1
        with get() = x.GetVar "c1"
        and set(value) = x.SetVar ("c1", value)
    member x.c2
        with get() = x.GetVar "c2"
        and set(value) = x.SetVar ("q2", value)
    member x.p1
        with get() = x.GetVar "p1"
        and set(value) = x.SetVar ("p1", value)
    member x.p2
        with get() = x.GetVar "p2"
        and set(value) = x.SetVar ("p2", value)
    member x.U
        with get() = x.GetVar "U"
        and set(value) = x.SetVar ("U", value)
    member x.K
        with get() = x.GetVar "K"
        and set(value) = x.SetVar ("K", value)
    member x.u
        with get() = x.GetFun<UtilityFunction> "u" 
        and set(value:UtilityFunction) = x.SetFun("u", value)
    member x.ExpectedUtility = x.U == x.p1 * x.u.[x.c1] + x.p2 * x.u.[x.c2]
