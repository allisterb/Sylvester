namespace Sylvester

open FSharp.Quotations
open FSharp.Quotations.DerivedPatterns

open Sylvester.Collections

open Arithmetic

type UnivariateDistribution<'t when 't:equality> = 
| ProbabilityMass of Expr<real->real>
| ProbabilityDensity of Expr<real->real>
    with
        member x.Func = 
            match x with
            | ProbabilityMass d
            | ProbabilityDensity d -> d
        member x.Transform(t:Expr<real->real>) =
            let vd = param_var x.Func
            let vt = param_var t
            let bodyt = t |> body |> subst_var_value vt (Expr.Var vd)
            let td = x.Func |> body |> subst_var_value vd bodyt |> recombine_func [vd] 
            match x with
            | ProbabilityMass _ -> ProbabilityMass <@ %%td:real->real @>
            | ProbabilityDensity _ -> ProbabilityMass <@ %%td:real->real @>

type MultivariateDistribution<'n, 't when 't:equality and 'n :> Number> = 
| JointProbability of Array<'n, UnivariateDistribution<'t>>
    
type ProbabilityDistribution<'n, 't when 'n :> Number and 't : equality> =
| UnivariateDistribution of UnivariateDistribution<'t>
| MultivariateDistribution of MultivariateDistribution<'n,'t>

type IRandomElement<'n, 't, 'd when 'n :> Number and 't : equality> = 
    abstract ProbabilityDistribution: ProbabilityDistribution<'n, 't>

type RandomVariable<'t when 't : equality>(map:Expr<'t -> real> option, support: Set<real> option, distr:UnivariateDistribution<'t> option, mean:Scalar<real> option) = 
    member val Map = map 
    member val Map' = Option.bind(evaluate >> Option.Some) map
    member val Support = defaultArg support RandomVariable<'t>.DefaultSupport
    member val Distribution = defaultArg distr (ProbabilityMass(<@ fun _ -> 0. @>))
    member x.ProbFunc = 
        fun (a:real) ->
            let v = param_var x.Distribution.Func
            let b = x.Distribution.Func |> body |> subst_var_value v (Expr.Value a) 
            <@ %%b:real @> |> Scalar<real>
    member x.Prob = fun a -> if a |?| x.Support then x.ProbFunc a else 0R
    
    interface IRandomElement<dim<1>, 't, real> with member x.ProbabilityDistribution = UnivariateDistribution(x.Distribution)
    interface ISet<real> with 
        member x.Set = x.Support
        member x.Equals b = x.Support.Equals b
    static member DefaultSupport = setOf Field.R
    
    
type Discrete<'t when 't : equality>(map:Expr<'t -> real> option, support:Set<real> option, pmf:Expr<real->real> option, mean:Scalar<real> option) = 
    inherit RandomVariable<'t>(map, support, pmf |> Option.bind (ProbabilityMass >> Some), mean)
    member x.Cdf = 
        fun i -> seq {0. .. i} |> Seq.map x.Prob |> Seq.reduce (+) 
    member x.Expectation = if mean.IsSome then mean.Value else x.Support |> Seq.map(fun e ->  e * (prob x e )) |> Seq.reduce (+)
    member x.Transform(t, m) = Discrete<'t>(map, support, Some (x.Distribution.Transform t).Func, Some m)
    static member (-) (l:Scalar<real>, r:Discrete<'t>) = r.Transform(<@ fun x -> %l.Expr - x @>, l - r.Expectation)


type Continuous<'t when 't : equality>(map:Expr<'t->real> option, support:Set<real> option, pdf:Expr<real->real> option, mean:Scalar<real> option) = 
    inherit RandomVariable<'t>(map, support, pdf |> Option.bind (integrate >> ProbabilityDensity >> Some), mean)
   
[<AutoOpen>]
module ProbabilityDistribution =
    let discrete<'t when 't : equality> s d = Discrete<'t>(None, Some s, Some d, None)

    let discrete_m<'t when 't : equality> s d m = Discrete<'t>(None, Some s, Some d, Some m)

    let continuous<'t when 't : equality> d = Continuous<'t>(None, None, Some d, None)

    let degenerate<'t when 't : equality> a = discrete<'t> (finite_seq [a]) <@ fun x -> 1. @>

    let uniform<'t when 't : equality> s = let l = (Seq.length s) in discrete<'t> (finite_seq s) <@ fun x -> 1. / real l  @>
    
    let poisson<'t when 't : equality> l = discrete_m<'t> (infinite_seq (fun i -> real i) |> Set.fromSeq) <@ fun x -> l ** x * (Math.e ** -l) / (factorial ((int) x)) @> (scalar l)

    let binomial<'t when 't : equality> p n = discrete<'t> ([0. .. real n] |> finite_seq) <@ fun x -> binomial_coeff n ((int) x) * ((p ** x) * ((1.-p) ** (real n - x))) @> 
    
    let bernoulli<'t when 't : equality> p = binomial p 1

    let geometric<'t when 't : equality> p n = discrete<'t> ([0. .. real n] |> finite_seq) <@ fun x -> ((1.-p) ** (x - 1.)) * p @>  