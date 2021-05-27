namespace Sylvester

open FSharp.Quotations

type ProbabilityDistribution<'t when 't:equality> = 
| ProbabilityMass of Expr<real->real>
| ProbabilityDensity of Expr<real->real>
| JointDistribution of ProbabilityDistribution<'t> * ProbabilityDistribution<'t>
with 
    member x.Expr = 
        match x with
        | ProbabilityMass d
        | ProbabilityDensity d -> d
        | JointDistribution(l, r) -> <@ fun x -> (%l.Expr) x + (%r.Expr) x @>
    member x.Copy a =
        match x with
        | ProbabilityMass _ -> ProbabilityMass a
        | ProbabilityDensity _ -> ProbabilityDensity a
        | _ -> failwith "Can only copy a probability mass or probability density function." 
type IRandomElement<'t, 'd when 't : equality> = interface end

type RandomVariable<'t when 't : equality>(map:Expr<'t -> real> option, support: Set<real> option, distr:ProbabilityDistribution<'t> option) = 
    member val Map = map 
    member val Map' = Option.bind(evaluate >> Option.Some) map
    member val Support = defaultArg support RandomVariable<'t>.DefaultSupport
    member val Distribution = defaultArg distr (ProbabilityMass(<@ fun _ -> 0. @>))
    member x.Prob  =
        fun a ->
            let v = param_var x.Distribution.Expr
            x.Distribution.Expr |> body |> subst_var_value v (Expr.Value a) |> expand''<real> |> Scalar
    interface IRandomElement<'t, real>
    static member DefaultSupport = setOf Field.R
    interface ISet<real> with 
        member x.Set = x.Support
        member x.Equals b = x.Support.Equals b
    static member (-) (l:real, r:RandomVariable<'t>) =
        let p =            
            let v = param_var r.Distribution.Expr
            r.Distribution.Expr |> body |> subst_var_value v (call_sub (Expr.Value l) (Expr.Var v)) |> recombine_func [v] 
            
        RandomVariable<'t>(r.Map, Some(r.Support), Some(r.Distribution.Copy <@ %%p:real->real @>))

type Discrete<'t when 't : equality>(?map:Expr<'t -> real>, ?support:Set<real>, ?pmf:Expr<real->real>) = 
    inherit RandomVariable<'t>(map, support, pmf |> Option.bind (ProbabilityMass >> Some))
     
type Continuous<'t when 't : equality>(?map:Expr<'t->real>, ?support:Set<real>, ?cdf:Expr<real->real>) = 
    inherit RandomVariable<'t>(map, support, cdf |> Option.bind (integrate >> ProbabilityDensity >> Some))
   
[<AutoOpen>]
module ProbabilityDistribution =
    let random<'t when 't : equality> s d = RandomVariable<'t>(None, Some s, Some d)

    let discrete<'t when 't : equality> s d = Discrete<'t>(support=s, pmf=d)

    let continuous<'t when 't : equality> d = Continuous<'t>(cdf=d)

    let poisson<'t when 't : equality> l n = discrete<'t> ([1. .. (real n)] |> finite_seq) <@ fun x -> l ** x * (Math.e ** -l) / (factorial ((int) x)) @>

    let binomial<'t when 't : equality> p n = discrete<'t> ([0. .. real n] |> finite_seq) <@ fun x -> binomial_coeff n ((int) x) * ((p ** x) * ((1.-p) ** (real n - x))) @> 
    
    let expectation (x:RandomVariable<_>) =
        match x with
        | :? Discrete<_> as d -> d.Support |> Seq.map(fun e ->  e * (prob d e )) |> Seq.reduce (+)
        | _ -> failwith "Can only compute expectation of a discrete or continuous variable."