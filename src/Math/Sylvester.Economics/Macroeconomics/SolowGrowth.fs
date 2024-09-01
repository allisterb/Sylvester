namespace Sylvester

type SolowGrowth() =
    inherit GrowthModel()
    do  
        base.CreateProdFun2("F", base.GetVar "K", base.GetVar "N")

    member x.F
        with get() = x.GetFun2<ProductionFunction2> "F"
        and set(value:ProductionFunction2) = x.SetFun2("F", value)

    member x.Output = x.Y == x.z * x.F.[x.K, x.N] 