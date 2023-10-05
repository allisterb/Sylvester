namespace Sylvester

open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns

(*
type EconomicFunction(a:Scalar<real>, ?symbol:string) = 
    inherit RealFunction(a, ?symbol=symbol)
    member val VarMap = a
    interface ISymbolic<EconomicFunction, real> with
        member a.Expr = a.Body
        member a.Symbol = a.Symbol
        member a.Transform(b:Expr<real>) = EconomicFunction(Scalar<real> b) 
        member a.TransformWithSymbol(b:Expr<real>, s:string) = EconomicFunction(Scalar<real> b, s)
type EconomicFunction2(a:Scalar<real>, ?symbol:string) = 
    inherit RealFunction2(a, ?symbol=symbol)
    member val VarMap = a
    interface ISymbolic<EconomicFunction2, real> with
       member a.Expr = a.Body
       member a.Symbol = a.Symbol
       member a.Transform(b:Expr<real>) = EconomicFunction2(Scalar<real> b) 
       member a.TransformWithSymbol(b:Expr<real>, s:string) = EconomicFunction2(Scalar<real> b, s)
 *)
 
type EconomicConstraint(a:ScalarRelation<real>) = 
    do 
        match a.Expr with
        | SpecificCall <@@ (=) @@> (_,_,lhs::rhs::[])
        | SpecificCall <@@ (>) @@> (_,_,lhs::rhs::[])
        | SpecificCall <@@ (<) @@> (_,_,lhs::rhs::[]) -> ()
        | _ -> failwithf "The expression %s is not a valid constraint expression." (src a.Expr)

(*
type DemandFunction(a: Scalar<real>, ?symbol:string) = 
    inherit EconomicFunction(a)

type SupplyFunction(a: Scalar<real>, ?symbol:string) = 
    inherit EconomicFunction(a)

type CostFunction(a: Scalar<real>, ?symbol:string) = 
    inherit EconomicFunction(a)

type UtilityFunction2(a: Scalar<real>, ?symbol:string) = 
    inherit EconomicFunction2(a)

type PPF(c: EconomicConstraint list) =
    member val Constraints = c
*)

type PPF(c: EconomicConstraint list) =
    member val Constraints = c

[<AutoOpen>]
module MicroEconomics =
    let marginal (func:ISymbolic<RealFunction, real>) = 
        match func.Symbol with
        | None -> diff (farg func.Term) func |> with_attr_tag "Marginal"
        | Some s -> (diff (farg func.Term) func) |> with_attr_tag "Marginal" |> with_symbol ("M" + s)

    let demandfun s (func:Scalar<real>) :RealFunction = RealFunction(func, s) |> with_attr_tag "DemandFunction"

    let supplyfun s (func:Scalar<real>) :RealFunction = RealFunction(func, s) |> with_attr_tag "SupplyFunction"

    let prodfun s (func:Scalar<real>) :RealFunction = RealFunction(func, s) |> with_attr_tag "ProductionFunction"

    let costfun s (func:Scalar<real>) :RealFunction = RealFunction(func, s) |> with_attr_tag "CostFunction"

    let ppf (c:ScalarRelation<real> list) = c |> List.map EconomicConstraint |> PPF