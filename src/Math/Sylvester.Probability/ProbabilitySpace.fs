namespace Sylvester

open FSharp.Quotations

type ProbabilityMeasure<'t when 't: equality> = Map<Set<'t>, real>

type ProbabilitySpace<'t when 't : equality>(set:Set<'t>, algebra:SigmaAlgebra<'t>, measure: ProbabilityMeasure<'t>) =
    member val Set = set
    member val Algebra = algebra
    member x.Measure(s:Set<'t>) = measure s
    member x.Measure(s:seq<'t>) = s |> Set.fromSeq |> measure
    interface ISet<'t> with 
        member val Set = set
        member a.Equals b = a.Set.Equals b
    new(set:Set<'t>, measure:ProbabilityMeasure<'t>) = ProbabilitySpace(set, SigmaAlgebra(set), measure)
    new(set:Set<'t>) = ProbabilitySpace(set, SigmaAlgebra(set), fun s -> if s |<| set  then (measure s) / (measure set) else 0.0)

[<AutoOpen>]
module ProbabilitySpace =
    let prob_space s = ProbabilitySpace s
    
    let prob_space_m s m = ProbabilitySpace(s, m)
    
    let prob_space_a_m s a m = ProbabilitySpace(s, a, m)
    
    let prob_measure (p:ProbabilitySpace<'t>) :(Set<'t>->real)= p.Measure

    let inline prob (p : ^T) x  =  (^T : (member Prob : (real->Scalar<real>)) (p)) <| (float) x

    let inline probi (p : ^T) a b  =  (^T : (member ProbInterval : (real->real->Scalar<real>)) (p)) <| a <| b

    let inline cprob (p : ^T) x  =  (^T : (member Cdf : (real->Scalar<real>)) (p)) <| (float) x

    let inline expectation (x : ^T) = (^T : (member Expectation : Scalar<real>) (x))

    let inline prob_expr (p : ^T) x  =  (^T : (member ProbExpr : (Expr<real>->Scalar<real>)) (p)) <|  x