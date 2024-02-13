namespace Sylvester

type ConsumerPreference() =
    inherit EconomicModel()
    do
        base.Vars.["q1"] <- realvar "q1"
        base.Vars.["q2"] <- realvar "q2"
        base.Vars.["Y"] <- realvar "Y"
    member x.q1
        with get() = x.GetVar "q1"
        and set(value) = x.SetVar ("q1", value)
    member x.q2
        with get() = x.GetVar "q2"
        and set(value) = x.SetVar ("q2", value)
    member x.p1
        with get() = x.GetVar "p1"
        and set(value) = x.SetVar ("p1", value)
    member x.p2
        with get() = x.GetVar "p2"
        and set(value) = x.SetVar ("p2", value)
    member x.Y
        with get() = x.GetVar "Y"
        and set(value) = x.SetVar ("Y", value)
    member x.U
        with get() = x.GetFun2<UtilityFunction2> "U" 
        and set(value:UtilityFunction2) = x.SetFun2("U", value)
    member x.BudgetConstraint = x.Y == x.p1 * x.q1 + x.p2 * x.q2