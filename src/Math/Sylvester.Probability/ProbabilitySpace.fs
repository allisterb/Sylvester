namespace Sylvester

type ProbabilityMeasure<'t when 't: equality> = Map<ISet<'t>, float>

type ProbabilitySpace<'t when 't : equality>(set:ISet<'t>, algebra:SigmaAlgebra<'t>, measure: ProbabilityMeasure<'t>) =
    member val Set = set.Set
    member val Algebra = algebra
    member x.Measure(s:ISet<'t>) = measure s
    member x.Measure(s:seq<'t>) = s |> Set.fromSeq |> measure
    interface ISet<'t> with 
        member val Set = set.Set
        member a.Equals b = a.Set.Equals b
    new(set:ISet<'t>, measure:ProbabilityMeasure<'t>) = ProbabilitySpace(set, SigmaAlgebra(set), measure)
    new(set:ISet<'t>) = ProbabilitySpace(set, SigmaAlgebra(set), fun s -> if s |<| set  then (measure s) / (measure set) else 0.0)

[<AutoOpen>]
module ProbabilitySpace =
    let prob_space s = ProbabilitySpace s
    let prob_space_m s m = ProbabilitySpace(s, m)
    let prob_space_a_m s a m = ProbabilitySpace(s, a, m)
    
    let prob_measure (p:ProbabilitySpace<'t>) :(ISet<'t>->real) = p.Measure