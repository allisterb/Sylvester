namespace Sylvester

open FSharp.Quotations
open FSharp.Quotations.DerivedPatterns

open Sylvester.Collections

open Arithmetic

type UnivariateDistribution = 
| ProbabilityMass of Expr<real->real> * Set<real> 
| ProbabilityDensity of Expr<real->real> * Set<real>
    with
        member x.Func = 
            match x with
            | ProbabilityMass(d, _) -> d
            | ProbabilityDensity(d, _) -> d
        member x.Support = 
            match x with
            | ProbabilityMass(_, s) -> s
            | ProbabilityDensity(_, s) -> s
        member x.Transform(t:Expr<real->real>, support:Set<real>) =
            let vd = param_var x.Func
            let vt = param_var t
            let bodyt = t |> body |> subst_var_value vt (Expr.Var vd)
            let td = x.Func |> body |> subst_var_value vd bodyt |> recombine_func [vd] 
            match x with
            | ProbabilityMass _ -> ProbabilityMass(<@ %%td:real->real @>, support)
            | ProbabilityDensity _ -> ProbabilityMass (<@ %%td:real->real @>, support)

type MultivariateDistribution<'n when 'n :> Number> = 
| JointProbability of Array<'n, UnivariateDistribution>
    
type ProbabilityDistribution<'n when 'n :> Number> =
| UnivariateDistribution of UnivariateDistribution
| MultivariateDistribution of MultivariateDistribution<'n>

type IRandomElement<'n, 'd when 'n :> Number> = 
    abstract ProbabilityDistribution: ProbabilityDistribution<'n>

type RandomVariable(distr:UnivariateDistribution option, mean:Scalar<real> option) = 
    member val Distribution = defaultArg distr (ProbabilityMass(<@ fun _ -> 0. @>, Set.Empty))
    interface IRandomElement<dim<1>, real> with member x.ProbabilityDistribution = UnivariateDistribution(x.Distribution)
    interface ISet<real> with 
        member x.Set = x.Distribution.Support
        member x.Equals b = x.Distribution.Support.Equals b
    
type Discrete(support:Set<real>, pmf:Expr<real->real>, mean:Scalar<real> option) = 
    inherit RandomVariable(Some(ProbabilityMass(pmf, support)), mean)
    member x.ProbFunc = 
        fun (a:real) ->
            let v = param_var x.Distribution.Func
            let b = x.Distribution.Func |> body |> subst_var_value v (Expr.Value a) 
            <@ %%b:real @>        
    member x.Prob = fun a -> if a |?| x.Distribution.Support then x.ProbFunc a |> Scalar<real> else 0R 
    member x.Cdf = 
        fun (i:real) -> seq {0. .. i} |> Seq.map x.Prob |> Seq.reduce (+) 
    member x.CProb = fun a -> if a |?| x.Distribution.Support then x.Cdf a else 0R 
    member x.Expectation = if mean.IsSome then mean.Value else x.Distribution.Support |> Seq.map(fun e ->  e * (prob x e )) |> Seq.reduce (+)
    member x.Transform(t, s, m) = Discrete(s, x.Distribution.Transform(t,s).Func, Some m)
   
    
    static member (-) (l:'a, r:Discrete) = let l' = real_expr l in r.Transform(<@ fun x -> %l' - x @>, (r.Distribution.Support |>| <@ fun x -> (%l' - x) |?| r.Distribution.Support @>), Scalar l' - r.Expectation)
    static member (-) (l:Discrete, r:'a) = let r' = real_expr r in l.Transform(<@ fun x -> x - %r' @>, (l.Distribution.Support |>| <@ fun x -> (x - %r') |?| l.Distribution.Support @>), Scalar r' - l.Expectation)

    static member (+) (l:'a, r:Discrete) = let l' = real_expr l in r.Transform(<@ fun x -> %l' + x @>, (r.Distribution.Support |>| <@ fun x -> (%l' + x) |?| r.Distribution.Support @>), Scalar l' + r.Expectation)
    static member (+) (l:Discrete, r:'a) = let r' = real_expr r in l.Transform(<@ fun x -> x + %r' @>, (l.Distribution.Support |>| <@ fun x -> (x + %r') |?| l.Distribution.Support @>), Scalar r' + l.Expectation)

    static member (*) (l:'a, r:Discrete) = let l' = real_expr l in r.Transform(<@ fun x -> %l' * x @>, (r.Distribution.Support |>| <@ fun x -> (%l' * x) |?| r.Distribution.Support @>), Scalar l' * r.Expectation)
    static member (*) (l:Discrete, r:'a) = let r' = real_expr r in l.Transform(<@ fun x -> x * %r' @>, (l.Distribution.Support |>| <@ fun x -> (x * %r') |?| l.Distribution.Support @>), Scalar r' * l.Expectation)

    static member (/) (l:'a, r:Discrete) = let l' = real_expr l in r.Transform(<@ fun x -> %l' / x @>, (r.Distribution.Support |>| <@ fun x -> (%l' / x) |?| r.Distribution.Support @>), Scalar l' / r.Expectation)
    static member (/) (l:Discrete, r:'a) = let r' = real_expr r in l.Transform(<@ fun x -> x / %r' @>, (l.Distribution.Support |>| <@ fun x -> (x / %r') |?| l.Distribution.Support @>), Scalar r' / l.Expectation)

type Continuous(support:Set<real>, pdf:Expr<real->real>, mean:Scalar<real> option) = 
    inherit RandomVariable(Some(ProbabilityDensity(pdf, support)), mean)
    member x.ProbFunc = 
        fun (a:real) ->         
            let v = param_var x.Distribution.Func
            let b = x.Distribution.Func |> integrate_over minf a |> sexpr |> subst_var_value v (Expr.Value a) 
            <@ %%b:real @>
    member x.Cdf = fun i -> Scalar<real>(x.ProbFunc i)
    member x.CProb = fun a -> if a |?| x.Distribution.Support then x.Cdf a else 0R 
    member x.ProbInterval = fun a b -> if a |?| x.Distribution.Support && b |?| x.Distribution.Support then (-) (Scalar<real>(x.ProbFunc b)) (Scalar<real>(x.ProbFunc a)) else 0R
    member x.Expectation = 
        if mean.IsSome then 
            mean.Value 
        else
            let v = param_var x.Distribution.Func
            let i = recombine_func [v] <| call_mul (Expr.Var v) (body x.Distribution.Func)
            integrate_over_R <@ %%i:real->real @>
    member x.Transform(t, s, m) = Continuous(s, (x.Distribution.Transform(t,s)).Func, Some m)
    
    static member (-) (l:'a, r:Continuous) = let l' = real_expr l in r.Transform(<@ fun x -> %l' - x @>, (r.Distribution.Support |>| <@ fun x -> (%l' - x) |?| r.Distribution.Support @>), Scalar l' - r.Expectation)
    static member (-) (l:Continuous, r:'a) = let r' = real_expr r in l.Transform(<@ fun x -> x - %r' @>, (l.Distribution.Support |>| <@ fun x -> (x - %r') |?| l.Distribution.Support @>), Scalar r' - l.Expectation)

    static member (+) (l:'a, r:Continuous) = let l' = real_expr l in r.Transform(<@ fun x -> %l' + x @>, (r.Distribution.Support |>| <@ fun x -> (%l' + x) |?| r.Distribution.Support @>), Scalar l' + r.Expectation)
    static member (+) (l:Continuous, r:'a) = let r' = real_expr r in l.Transform(<@ fun x -> x + %r' @>, (l.Distribution.Support |>| <@ fun x -> (x + %r') |?| l.Distribution.Support @>), Scalar r' + l.Expectation)

    static member (*) (l:'a, r:Continuous) = let l' = real_expr l in r.Transform(<@ fun x -> %l' * x @>, (r.Distribution.Support |>| <@ fun x -> (%l' * x) |?| r.Distribution.Support @>), Scalar l' * r.Expectation)
    static member (*) (l:Continuous, r:'a) = let r' = real_expr r in l.Transform(<@ fun x -> x * %r' @>, (l.Distribution.Support |>| <@ fun x -> (x * %r') |?| l.Distribution.Support @>), Scalar r' * l.Expectation)

    static member (/) (l:'a, r:Continuous) = let l' = real_expr l in r.Transform(<@ fun x -> %l' / x @>, (r.Distribution.Support |>| <@ fun x -> (%l' / x) |?| r.Distribution.Support @>), Scalar l' / r.Expectation)
    static member (/) (l:Continuous, r:'a) = let r' = real_expr r in l.Transform(<@ fun x -> x / %r' @>, (l.Distribution.Support |>| <@ fun x -> (x / %r') |?| l.Distribution.Support @>), Scalar r' / l.Expectation)
   
[<AutoOpen>]
module ProbabilityDistribution =
    let discrete s d = Discrete(s, d, None)

    let discrete_m s d m = Discrete(s, d, Some m)

    let continuous s d = Continuous(s, d, None)

    let degenerate a = discrete (finite_seq [a]) <@ fun x -> 1. @>

    let uniform s = let l = (Seq.length s) in discrete (finite_seq s) <@ fun x -> 1. / real l  @>
    
    let poisson l = discrete_m (infinite_seq (fun i -> real i) |> Set.fromSeq) <@ fun x -> l ** x * (Math.e ** -l) / (factorial ((int) x)) @> (scalar l)

    let binomial p n = discrete ([0. .. real n] |> finite_seq) <@ fun x -> binomial_coeff n ((int) x) * ((p ** x) * ((1.-p) ** (real n - x))) @> 
    
    let bernoulli<'t when 't : equality> p = binomial p 1

    let geometric<'t when 't : equality> p n = discrete ([0. .. real n] |> finite_seq) <@ fun x -> ((1.-p) ** (x - 1.)) * p @>
    
    let uniform_continuous<'t when 't : equality> a b = 
        let a',b' = real_expr a, real_expr b
        continuous (open_interval a b) <@ fun _ -> 1. / (%b' - %a') @> 