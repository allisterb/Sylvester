namespace Sylvester

type MalthusianGrowth() =
    inherit GrowthModel()
    member x.g
        with get() = x.GetFun<RealFunction> "g"
        and set(value:RealFunction) = x.SetFun("g", value)
    member x.PopulationGrowth = x.N' / x.N == x.g.[x.c]
    member x.Output = x.Y == x.z * x.F.[x.L, x.N]
    member x.ConstantReturnsToScale = x.z * x.F.[(1 / x.N) * x.L, (1 / x.N) * x.N] == x.z * (1 / x.N) * x.F.[x.L, x.N]
    member x.IncomeExpenditure = x.C == x.Y
    override x.Constraints = base.Constraints @ [x.PopulationGrowth; x.Output; x.ConstantReturnsToScale; x.IncomeExpenditure]
    
type MalthusianGrowthView =
| PopulationGrowth
