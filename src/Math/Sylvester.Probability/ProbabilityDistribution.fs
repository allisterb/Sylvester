namespace Sylvester

open FSharp.Quotations

type IRandomElement<'t, 'd> = interface end

type ProbabilityMass<'t when 't : equality> = Expr<'t -> real>

type ProbabilityDensity<'t when 't : equality> = Expr<'t -> real>

type RandomVariable<'t when 't : equality>(space: ProbabilitySpace<'t>, map:Expr<'t -> real>, ?support: Set<real> ) = 
    member val Space = space
    member val Map = map
    member val Support = defaultArg support Field.R.Set
    
type Discrete<'t when 't : equality>(space:ProbabilitySpace<'t>, map:ProbabilityMass<'t>, ?support:Set<real>) = inherit RandomVariable<'t>(space, map, defaultArg support Field.R.Set)

type Continuous<'t when 't : equality>(space:ProbabilitySpace<'t>, map:ProbabilityDensity<'t>, ?support:Set<real>) = inherit RandomVariable<'t>(space, map, defaultArg support Field.R.Set)