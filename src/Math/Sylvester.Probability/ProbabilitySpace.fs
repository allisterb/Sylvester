﻿namespace Sylvester

type ProbabilityMeasure<'t when 't: equality> = Map<Set<'t>, float>

type ProbabilitySpace<'t when 't : equality>(set:Set<'t>, algebra:SigmaAlgebra<'t>, measure: ProbabilityMeasure<'t>) =
    member val Set = set
    member val Algebra = algebra
    member x.Measure(s:seq<'t>) = s |> Set.fromSeq |> measure
    interface ISet<'t> with 
        member val Set = set
        member a.Equals b = a.Set.Equals b
    new(set:Set<'t>, measure:ProbabilityMeasure<'t>) = ProbabilitySpace(set, SigmaAlgebra(set), measure)
    new(set:Set<'t>) = ProbabilitySpace(set, SigmaAlgebra(set), fun s -> if set.HasSubset s then (s.Length |> float) / (set.Length |> float) else 0.0)