namespace Sylvester

open FSharp.Quotations

type ProbabilityMeasure<'t when 't: equality>(event_space:SigmaAlgebra<'t>, prob_function:MapExpr<Set<'t>, real>) =
    inherit SetFunction<'t>(event_space.Subsets.Set, closed_interval 0. 1., prob_function)
    override x.ScalarExpr = 0R
    override x.ScalarVars = []

type ProbabilitySpace<'t when 't : equality>(event_space:SigmaAlgebra<'t>, prob_function:MapExpr<Set<'t>, real>) =
    member val SampleSpace = event_space.Set
    member val EventSpace = event_space
    member val ProbMeasure = ProbabilityMeasure<'t>(event_space, prob_function)
    member x.Measure(v:Set<'t>) = x.ProbMeasure.[v]
    member x.Item(v:Set<'t>)  = x.Measure(v)
   
    interface ISet<Set<'t>> with 
        member val Set = event_space.Subsets.Set
        member a.Equals b = a.EventSpace.Subsets.Set = b

    new (set:ISet<'t>) = ProbabilitySpace<'t>(SigmaAlgebra<'t>(set), <@ fun s -> measure s / measure set @>)
    
[<AutoOpen>]
module ProbabilitySpace =
    let prob_event<'t when 't : equality> (sample_space:ISet<'t>) (evt:obj) =
        match evt with
        | :? 't as e ->  ProbabilityEvent<'t>(sample_space, finite_seq [e])
        | :? Set<'t> as s -> ProbabilityEvent<'t>(sample_space, s)
        | :? seq<'t> as se -> ProbabilityEvent<'t>(sample_space, se |> finite_seq)
        | _ -> failwith ""
    
    let prob_space<'t when 't : equality> (s:ISet<'t>) = ProbabilitySpace<'t> (s.Set)

    let prob_space_f<'t when 't : equality> (s:ISet<'t>) (prob_function:MapExpr<Set<'t>, real>) = ProbabilitySpace (SigmaAlgebra<'t>(s), prob_function)

    let inline prob (p : ^T) x  =  (^T : (member Prob : (real->Scalar<real>)) (p)) <| (real) x

    let inline probi (p : ^T) a b  =  (^T : (member ProbInterval : (real->real->Scalar<real>)) (p)) <| a <| b

    let inline cprob (p : ^T) x  =  (^T : (member Cdf : (real->Scalar<real>)) (p)) <| (float) x

    let inline expectation (x : ^T) = (^T : (member Expectation : Scalar<real>) (x))

    let inline prob_expr (p : ^T) x  =  (^T : (member ProbExpr : (Expr<real>->Scalar<real>)) (p)) <|  x