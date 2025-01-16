namespace Sylvester

[<AbstractClass>]
type GrowthModel() =
    inherit EconomicModel()
    do base.CreateVars ("N", "N'", "K", "K'", "L", "C", "Y", "l", "c", "y", "z")
        
    member x.N 
        with get() = x.GetVar "N"
        and set(value) = x.SetVar("N", value)
    member x.N' 
        with get() = x.GetVar "N'"
        and set(value) = x.SetVar("N'", value)
    member x.L 
           with get() = x.GetVar "L"
           and set(value) = x.SetVar("L", value)
    member x.K 
           with get() = x.GetVar "K"
           and set(value) = x.SetVar("K", value)
    member x.C 
        with get() = x.GetVar "C"
        and set(value) = x.SetVar("C", value)
    member x.c 
        with get() = x.GetVar "c"
        and set(value) = x.SetVar("c", value)
    member x.l 
        with get() = x.GetVar "l"
        and set(value) = x.SetVar("l", value)
    member x.Y 
           with get() = x.GetVar "Y"
           and set(value) = x.SetVar("Y", value)
    member x.y 
        with get() = x.GetVar "y"
        and set(value) = x.SetVar("y", value)
    member x.z 
        with get() = x.GetVar "z"
        and set(value) = x.SetVar("z", value)
    member x.F
        with get() = x.GetFun2<ProductionFunction2> "F"
        and set(value:ProductionFunction2) = x.SetFun2("F", value)
    member x.PerWorkerOutput = x.y == x.Y / x.N
    member x.PerWorkerConsumption = x.c == x.C / x.N
    member x.PerWorkerLand = x.l == x.L / x.N
    override x.Constraints = [x.PerWorkerConsumption]