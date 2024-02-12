namespace Sylvester

[<AbstractClass>]
type GrowthModel() =
    inherit EconomicModel()
    do
        base.Vars.["g"] <- realvar "g"
        base.Vars.["N"] <- realvar "N"
        base.Vars.["N'"] <- realvar "N'"
        base.Vars.["C"] <- realvar "C"
        base.Vars.["Y"] <- realvar "Y"
        base.Vars.["s"] <- realvar "s"
        base.Vars.["K"] <- realvar "K"
        base.Vars.["L"] <- realvar "L"
    member x.N 
        with get() = x.GetVar "N"
        and set(value) = x.SetVar("N", value)
    member x.N' 
        with get() = x.GetVar "N'"
        and set(value) = x.SetVar("N'", value)
    member x.C 
        with get() = x.GetVar "C"
        and set(value) = x.SetVar("C", value)
    member x.c 
        with get() = x.GetVar "c"
        and set(value) = x.SetVar("c", value)
    member x.g
        with get() = x.GetFun<RealFunction> "g"
        and set(value:RealFunction) = x.SetFun("g", value)
    abstract PopulationGrowth:RealFunction
