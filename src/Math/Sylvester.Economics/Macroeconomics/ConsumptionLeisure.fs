namespace Sylvester

type ConsumptionLeisure() =
    inherit EconomicModel() 
    do 
        base.CreateVars("n", "C", "l", "W", "t", "h", "T", "pi")
        base.CreateVar("Ns", "N^s")
        
    member x.n 
        with get() = x.GetVar("n")
        and set(value) = x.SetVar("n", value)
    member x.C 
        with get() = x.GetVar "C"
        and set(value) = x.SetVar("C", value)
    member x.l
        with get() = x.GetVar "l"
        and set(value) = x.SetVar("l", value)
    member x.W 
        with get() = x.GetVar "W"
        and set(value) = x.SetVar("W", value)
    member x.t 
        with get() = x.GetVar("t")
        and set(value) = x.SetVar("t", value)
    member x.h 
        with get() = x.GetVar("h")
        and set(value) = x.SetVar("h", value)
    member x.Ns
        with get() = x.GetVar("Ns")
        and set(value) = x.SetVar("Ns", value)
    member x.T
        with get() = x.GetVar("T")
        and set(value) = x.SetVar("T", value)
    member x.pi
        with get() = x.GetVar("pi")
        and set(value) = x.SetVar("pi", value)
    member x.U
        with get() = x.GetFun2<UtilityFunction2> "U" 
        and set(value:UtilityFunction2) = x.SetFun2("U", value)
    
    member val Tax:Tax option = Some LumpSum with get,set 
    member x.UtilityConstraints = x.U.ScalarVars
    member x.TimeConstraint = x.l + x.Ns == x.h 
    member x.BudgetConstraint = x.C == x.W * x.Ns + x.pi - x.T

type ConsumerLeisureDiagram = {
    View:ConsumerLeisureView
    Fix:obj
}
and ConsumerLeisureView =
| Normal

