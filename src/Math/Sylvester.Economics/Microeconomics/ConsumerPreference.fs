namespace Sylvester

open Economics

type ConsumerPreference() =
    inherit EconomicModel()
    do
        base.CreateVar("Y")
        base.CreateVars(("q1", "q_1"), ("q2", "q_2"), ("p1", "p_1"), ("p2", "p_2)"))
        base.CreateUtilFun2("U", "q1", "q2")

    member x.q1
        with get() = x.GetVar "q1" and set(value) = x.SetVar ("q1", value)
    member x.q2
        with get() = x.GetVar "q2" and set(value) = x.SetVar ("q2", value)
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
    member x.UtilityMaximization = mrs x.U == x.p1 / x.p2
    override x.Constraints = [x.BudgetConstraint; x.UtilityMaximization]
    
    member x.DemandFunctions =
        let q = solve {|posvars=true|} [x.q1;x.q2] x.Equations
        do if q.Length <> 2 then failwithf "Could not solve constraints for %A and %A." x.q1 x.q2
        [demandfun "q1" (fixvar [x.p2; x.Y] (rhs q.[0])); demandfun "q2" (fixvar [x.p1; x.Y] (rhs q.[1]))]

    interface IWebVisualization with
        member x.Draw(attrs:_) = 
            let view = if has_prop<ConsumerPreferenceView> "View" attrs then get_prop<ConsumerPreferenceView> "View" attrs else failwith "A view must be specified for this consumer preference diagram"
            
            match view with
            | UtililtyMaximization ->
                let uvals = if has_prop<seq<real>> "U" attrs then get_prop<seq<real>> "U" attrs else failwith "You must specify some utility values to plot indifference curves." 
                let fs = uvals |> Seq.map(fun v -> utilfun_im (sprintf "%s = %A" x.q2.Name v) x.q2 (x.U == v) :> IRealFunction<RealFunction>)
                //let Y = get_prop_else<real*real> "Y" (0.,10.) attrs 
                //let p1 = get_prop_else<real*real> "p1" (0.,10.) attrs
                //let p2 = get_prop_else<real*real> "p2" (0.,10.) attrs

                WebVisualization.draw_realfuns attrs (fs |> Seq.map(fun x->x.Html()) |> Seq.toArray) (fs |> Seq.map(fun x ->x.Term.MapExpr) |> Seq.toArray) |> draw_board
                
                //to_json dict |> ignore
            //Html.Text "kk"    
            
and ConsumerPreferenceView =
| UtililtyMaximization
            
   