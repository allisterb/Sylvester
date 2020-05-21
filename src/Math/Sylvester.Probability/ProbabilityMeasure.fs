namespace Sylvester

type MeasureOp<'t when 't: equality> = Map<Set<'t>, float>

type ProbabilitySpace<'t when 't : equality>(set:Set<'t>, algebra:SigmaAlgebra<'t>, measure: MeasureOp<'t>) =
    member val Set = set
    member val Algebra = algebra
    member val Measure = measure
    member x.Prob(s:Set<'t>) = x.Measure s
    new(set:Set<'t>, measure:MeasureOp<'t>) = ProbabilitySpace(set, set |> Set.toSigmaAlgebra, measure)
    new(set:Set<'t>) = ProbabilitySpace(set, set |> Set.toSigmaAlgebra, fun s -> (s.Length |> float) / (set.Length |> float))