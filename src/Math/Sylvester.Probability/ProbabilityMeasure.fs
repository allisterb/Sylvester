namespace Sylvester

type MeasureOp<'t when 't: equality> = Map<Set<'t>, float>

type ProbabilityMeasure<'t when 't : equality>(algebra:SigmaAlgebra<'t>, measure: MeasureOp<'t>) =
    member val Algebra = algebra
    member val Measure = measure
    member x.Prob(s:Set<'t>) = x.Measure s
    new(set:ISet<'t>, measure:MeasureOp<'t>) = ProbabilityMeasure(set.Set.AsSigmaAlgebra, measure)

[<AutoOpen>]
module ProbabilityMeasure = 
    let probabilityMeasure (s:SigmaAlgebra<_>) (m:MeasureOp<_>) = ProbabilityMeasure(s, m)