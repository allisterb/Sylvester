namespace Sylvester

type ConsumerLeisure() =
    inherit EconomicModel() 
    do
        let c,l,W,t = realvar4 "c" "l" "W" "t"
        base.Vars.["c"] <- c
        base.Vars.["l"] <- l
        base.Vars.["W"] <- W
        base.Vars.["t"] <- t
    member x.c 
        with get() = x.GetVar "c"
        and set(value) = x.SetVar("c", value)
    member x.l
           with get() = x.GetVar "l"
           and set(value) = x.SetVar("l", value)
    member x.W 
           with get() = x.GetVar "W"
           and set(value) = x.SetVar("W", value)
    member x.t 
           with get() = x.GetVar("t")
           and set(value) = x.SetVar("t", value)
    member x.n 
           with get() = x.GetVar("n")
           and set(value) = x.SetVar("n", value)
    member x.h 
           with get() = x.GetVar("h")
           and set(value) = x.SetVar("h", value)
    member x.Ns
              with get() = x.GetVar("N^s")
              and set(value) = x.SetVar("N^s", value)
    member x.U
        with get() = x.GetFun2<UtilityFunction2> "U" 
        and set(value:UtilityFunction2) = x.SetFun2("U", value)
        
