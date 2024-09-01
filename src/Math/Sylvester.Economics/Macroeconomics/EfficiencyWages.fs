
namespace Sylvester

open Economics

type EfficiencyWages() =
    inherit EconomicModel()
    do
        base.CreateVar("N")
        base.CreateVar("E")
        base.CreateVar("w")
        base.CreateVar("Pi")

        base.CreateFun("e", base.Vars.["w"])
        base.CreateProdFun("F", base.Vars.["E"])

    member x.N 
           with get() = x.GetVar "N"
           and set(value) = x.SetVar("N", value)
    member x.E 
        with get() = x.GetVar "E"
        and set(value) = x.SetVar("E", value)
    member x.w 
        with get() = x.GetVar "w"
        and set(value) = x.SetVar("w", value)
    member x.Pi 
        with get() = x.GetVar "Pi"
        and set(value) = x.SetVar("Pi", value)
    member x.e 
        with get() = x.GetFun<RealFunction> "w"
        and set(value) = x.SetFun<RealFunction>("w", value)
    member x.F 
        with get() = x.GetFun<ProductionFunction> "F"
        and set(value) = x.SetFun<ProductionFunction>("F", value)

    member x.Efficiency = x.E == x.e.[x.w] * x.N
    member x.Profit = x.Pi == x.F.[x.E]  - (x.w / x.e.[x.w]) * x.E
    override x.Constraints = base.Constraints @ [x.Efficiency; x.Profit]
        