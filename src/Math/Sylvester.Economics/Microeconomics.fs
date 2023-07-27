namespace Sylvester

open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns

type EconomicFunction(a:ScalarVarMap<real>) = 
    inherit RealFunction(a.Rhs)
    member val VarMap = a
    interface ISymbolic<EconomicFunction, real> with
        member a.Expr = a.Body
        member a.Mutate(b:Expr<real>) = EconomicFunction <| ScalarVarMap<real>(a.VarMap.Var, Scalar<real> b) 
    
type EconomicConstraint(a:ScalarRelation<real>) = 
    do 
        match a.Expr with
        | SpecificCall <@@ (=) @@> (_,_,lhs::rhs::[])
        | SpecificCall <@@ (>) @@> (_,_,lhs::rhs::[])
        | SpecificCall <@@ (<) @@> (_,_,lhs::rhs::[]) -> ()
        | _ -> failwithf "The expression %s is not a valid constraint expression." (src a.Expr)

type DemandFunction(a: ScalarVarMap<real>) = 
    inherit EconomicFunction(a)

type SupplyFunction(a: ScalarVarMap<real>) = 
    inherit EconomicFunction(a)

type CostFunction(a: ScalarVarMap<real>) = 
    inherit EconomicFunction(a)

type PPF(c: EconomicConstraint list) =
    member val Constraints = c

[<AutoOpen>]
module MicroEconomics =
    let marginal (func:EconomicFunction) :EconomicFunction = diff func.Arg func
    
    let demand func = DemandFunction func

    let supply func = SupplyFunction func

    let cost func = CostFunction func

    let revenue func = EconomicFunction func

    let plot_name (func:EconomicFunction) = Expr.Value((sprintf "q_D(%A) = %s" (farg func) (sprinte <| fexpr func)) :> obj)

    let ppf (c:ScalarRelation<real> list) = c |> List.map EconomicConstraint |> PPF