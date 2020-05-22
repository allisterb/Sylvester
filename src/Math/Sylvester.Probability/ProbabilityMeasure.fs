namespace Sylvester

type ProbabilityMeasureOp<'t when 't: equality> = Map<Set<'t>, float>

type ProbabilitySpace<'t when 't : equality>(set:Set<'t>, algebra:SigmaAlgebra<'t>, measure: ProbabilityMeasureOp<'t>) =
    member val Set = set
    member val Algebra = algebra
    member val Measure = measure

    new(set:Set<'t>, measure:ProbabilityMeasureOp<'t>) = ProbabilitySpace(set, set |> Set.toSigmaAlgebra, measure)
    new(set:Set<'t>) = ProbabilitySpace(set, set |> Set.toSigmaAlgebra, fun s -> if set.HasSubset s then (s.Length |> float) / (set.Length |> float) else 0.0)

    member x.Prob(s:Set<'t>) = x.Measure s
    member x.Prob'(s:seq<'t>) = s |> Set.fromSeq |> x.Measure

module ProbabilitySpace = 
    let prob (s:ProbabilitySpace<'t>) = s.Prob
    let prob' (s:ProbabilitySpace<'t>) = s.Prob'