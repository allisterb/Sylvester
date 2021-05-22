namespace Sylvester

open System

open FSharp.Quotations

type IRandomElement<'t, 'd> = 
    abstract Distribution:ProbabilityDistribution 
    
and ProbabilityDistribution = Expr<real -> real>

type ProbabilityMass = Expr<real -> real>

type ProbabilityDensity = Expr<real -> real>

[<AutoOpen>]
module ProbabilityDistribution =
    let poisson = <@ fun (l:real) (x:real) -> (l ** x) * e ** -l @> 
    
[<AbstractClass>]
type RandomVariable<'t when 't : equality>(support: Set<real> option, map:Expr<'t -> real> option) = 
    member val Support = defaultArg support (setOf Field.R)
    member val Map = map 
    member val Map' = Option.bind(evaluate >> Option.Some) map
    abstract Distribution:ProbabilityDistribution
    interface IRandomElement<real, real> with
        member x.Distribution = x.Distribution
    static member DefaultSupport = setOf Field.R

type Discrete<'t when 't : equality>(d:ProbabilityMass, ?support:Set<real>, ?map:Expr<'t -> real>) = 
    inherit RandomVariable<'t>(support, map)
    override x.Distribution = d

type Continuous<'t when 't : equality>(d:ProbabilityDensity, ?support:Set<real>, ?map:Expr<'t->real>) = 
    inherit RandomVariable<'t>(support, map)
    override x.Distribution = integrate d (d |> param_vars |> List.last |> Expr.Var |> expand''<real>)
