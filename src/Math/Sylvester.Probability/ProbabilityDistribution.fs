namespace Sylvester

open FSharp.Quotations

open Arithmetic

type IUnivariateDistribution =
    abstract member Support: Set<real>
    abstract member Func: RealFunction
    abstract member Prob: a:int->Scalar<real>
    abstract member Prob: a:real->Scalar<real>
    abstract member Prob: a:Scalar<real>->Scalar<real>
    abstract member CProb: a:int*int->Scalar<real>
    abstract member CProb: a:real*real->Scalar<real>
    abstract member Transform: support:ISet<real>*t:Expr<real->real>*((real->real) option)->IUnivariateDistribution

type DiscreteProbabilityDistribution(support:ISet<real>, pmf:Expr<real->real>, ?probmap:real->real) = 
    member x.Support = support.Set 
    member x.Pmf = realfun_l pmf
    member x.ProbMap = defaultArg probmap (ev pmf)
    member x.Prob(a:int) = x.ProbMap (real a) |> Scalar
    member x.Prob(a:real) = x.ProbMap a |> Scalar
    member x.Prob(a:Scalar<real>) = x.Pmf.[a]
    member x.CProb(a:int, b:int) = [a .. b] |> Seq.map x.Prob |> Seq.reduce (+) |> simplify
    member x.CProb(a:real, b:real) = [a .. b] |> Seq.map x.Prob |> Seq.reduce (+) |> simplify
    member x.Transform(support:ISet<real>, t:Expr<real->real>, ?probmap:real->real) = 
        let vd = param_var x.Pmf.MapExpr
        let vt = param_var t
        let bodyt = t |> body |> subst_var_value vt (Expr.Var vd)
        let td = x.Pmf.MapExpr |> body |> subst_var_value vd bodyt |> recombine_func_as<real->real> [vd] 
        DiscreteProbabilityDistribution(support, td, ?probmap=probmap)
    member x.Item(a:int) = x.Prob a
    member x.Item(a:real) = x.Prob a
    member x.Item(a:Scalar<real>) = x.Prob a
    member x.Item(a: int, b:int) = x.CProb(a, b)
    member x.Item(a: real, b:real) = x.CProb(a, b)
    interface IUnivariateDistribution with
        member x.Support = x.Support
        member x.Func = x.Pmf
        member x.Prob(a:int) = x.Prob a
        member x.Prob(a:real) = x.Prob a
        member x.Prob(a:Scalar<real>) = x.Prob a
        member x.CProb(a:int, b:int) = x.CProb(a,b)
        member x.CProb(a:real,b:real) = x.CProb(a,b)
        member x.Transform(support:ISet<real>, t:Expr<real->real>, probmap:(real->real) option) = x.Transform(support, t, ?probmap=probmap) :> IUnivariateDistribution
            
type ContinuousProbabilityDistribution(support:ISet<real>, pdf:Expr<real->real>, ?probmap:real->real) = 
    member x.Support = support.Set 
    member x.Pdf = realfun_l pdf
    member x.CProb a = integrate_fun_over minf a x.Pdf
    member x.CProbMap = defaultArg probmap (fun (a:real) -> ev (x.CProb(a).Expr))
    member x.Transform(support:ISet<real>, t:Expr<real->real>, ?probmap:real->real) = 
        let vd = param_var pdf
        let vt = param_var t
        let bodyt = t |> body |> subst_var_value vt (Expr.Var vd)
        let pd = Ops.Integrate <@ %%Expr.Var(vd): real @> (pdf |> body |> expand_as<real>) 
        let td = pd |> subst_var_value vd bodyt |> expand_as<real>
        let tpd = td |> Ops.Diff 1 <@ %%Expr.Var(vd): real @> |> recombine_func_as<real->real> [vd]
        ContinuousProbabilityDistribution(support, tpd, ?probmap=probmap)        
    member x.Item(a:int) = x.CProbMap (real a)
    member x.Item(a:real) = x.CProbMap a
    member x.Item(a:Scalar<real>) = x.CProb a

    interface IUnivariateDistribution with
        member x.Support = x.Support
        member x.Func = x.Pdf
        member x.Prob(a:int) = 0R
        member x.Prob(a:real) = 0R
        member x.Prob(a:Scalar<real>) = 0R
        member x.CProb(a:int, b:int) = x.CProb(a, b)
        member x.CProb(a:real, b:real) = x.CProb(a, b)
        member x.Transform(support:ISet<real>, t:Expr<real->real>, probmap:(real->real) option) = x.Transform(support, t, ?probmap=probmap) :> IUnivariateDistribution

type MultivariateDistribution = 
| JointProbability of IUnivariateDistribution[]
    
type ProbabilityDistribution =
| UnivariateDistribution of IUnivariateDistribution
| MultivariateDistribution of MultivariateDistribution

type IRandomElement<'n, 'd when 'n :> Number> = 
    abstract ProbabilityDistribution: ProbabilityDistribution

type IRandomVariable = 
    inherit IRandomElement<dim<1>, real> 
    inherit ISet<real>

type DiscreteRandomVariable(distr: DiscreteProbabilityDistribution, ?mean:Scalar<real>) =      
    member x.Distribution = distr
    member x.Prob (a:int) = let a' = real a in if a' |?| x.Distribution.Support then x.Distribution.ProbMap a' else 0. 
    member x.Prob (a:real) = if a |?| x.Distribution.Support then x.Distribution.ProbMap a else 0. 
    member x.Prob (a:Scalar<real>) = x.Distribution.Pmf.[a] 
    member x.Cdf(i:real) = [0.0 .. i] |> Seq.choose(fun e-> if x.Prob e = 0. then Some e else None) |> Seq.map x.Prob |> Seq.reduce (+)
    member x.CProb(a:real) = if a |?| x.Distribution.Support then x.Cdf a else 0. 
    member x.Expectation = if mean.IsSome then mean.Value else x.Distribution.Support |> Seq.map(fun e ->  e * (x.Prob e )) |> Seq.reduce (+) |> Scalar |> simplify
    member x.Transform(s, t, ?m) = DiscreteRandomVariable(distr.Transform(s, t, distr.ProbMap), ?mean=m)
    member x.Item(a: int) = x.Prob a
    member x.Item(a: real) = x.Prob a
    member x.Item(a: Scalar<real>) = x.Prob a
    
    interface IRandomVariable with member x.ProbabilityDistribution = UnivariateDistribution(x.Distribution)

    interface ISet<real> with 
        member x.Set = x.Distribution.Support
        member x.Equals b = x.Distribution.Support.Equals b
    
    static member (-) (l:'a, r:DiscreteRandomVariable) = let l' = realexpr l in r.Transform((r.Distribution.Support |>| <@ fun x -> (%l' - x) |?| r.Distribution.Support @>), <@ fun x -> %l' - x @>, Scalar l' - r.Expectation)
    static member (-) (l:DiscreteRandomVariable, r:'a) = let r' = realexpr r in l.Transform((l.Distribution.Support |>| <@ fun x -> (x - %r') |?| l.Distribution.Support @>), <@ fun x -> x - %r' @>, Scalar r' - l.Expectation)

    static member (+) (l:'a, r:DiscreteRandomVariable) = let l' = realexpr l in r.Transform((r.Distribution.Support |>| <@ fun x -> (%l' - x) |?| r.Distribution.Support @>), <@ fun x -> %l' + x @>, Scalar l' + r.Expectation)
    static member (+) (l:DiscreteRandomVariable, r:'a) = let r' = realexpr r in l.Transform((l.Distribution.Support |>| <@ fun x -> (x + %r') |?| l.Distribution.Support @>), <@ fun x -> x + %r' @>, Scalar r' + l.Expectation)

    static member (*) (l:'a, r:DiscreteRandomVariable) = let l' = realexpr l in r.Transform((r.Distribution.Support |>| <@ fun x -> (%l' * x) |?| r.Distribution.Support @>), <@ fun x -> %l' * x @>, Scalar l' * r.Expectation)
    static member (*) (l:DiscreteRandomVariable, r:'a) = let r' = realexpr r in l.Transform((l.Distribution.Support |>| <@ fun x -> (x * %r') |?| l.Distribution.Support @>), <@ fun x -> x * %r' @>, Scalar r' * l.Expectation)

    static member (/) (l:'a, r:DiscreteRandomVariable) = let l' = realexpr l in r.Transform( (r.Distribution.Support |>| <@ fun x -> (%l' / x) |?| r.Distribution.Support @>), <@ fun x -> %l' / x @>, Scalar l' / r.Expectation)
    static member (/) (l:DiscreteRandomVariable, r:'a) = let r' = realexpr r in l.Transform((l.Distribution.Support |>| <@ fun x -> (x / %r') |?| l.Distribution.Support @>), <@ fun x -> x / %r' @>, Scalar r' / l.Expectation)

    static member (</) (l:DiscreteRandomVariable, r:real) = l.Cdf r
    
type ContinuousRandomVariable(distr:ContinuousProbabilityDistribution, ?mean:Scalar<real>) = 
    member val Distribution = distr
    member x.ProbInterval(a:Scalar<real>, b:Scalar<real>)= x.Distribution.CProb b - x.Distribution.CProb a 
    member x.Expectation = 
        if mean.IsSome then 
            mean.Value 
        else
            integrate_fun_over_R (x.Distribution.Pdf.ScalarVar * x.Distribution.Pdf)
    member x.Transform(s, t, m) = ContinuousRandomVariable(distr.Transform(s,t), m)
    
    static member (-) (l:'a, r:ContinuousRandomVariable) = let l' = realexpr l in r.Transform((r.Distribution.Support |>| <@ fun x -> (%l' - x) |?| r.Distribution.Support @>), <@ fun x -> %l' - x @>, Scalar l' - r.Expectation)
    static member (-) (l:ContinuousRandomVariable, r:'a) = let r' = realexpr r in l.Transform((l.Distribution.Support |>| <@ fun x -> (x - %r') |?| l.Distribution.Support @>), <@ fun x -> x - %r' @>, Scalar r' - l.Expectation)

    static member (+) (l:'a, r:ContinuousRandomVariable) = let l' = realexpr l in r.Transform((r.Distribution.Support |>| <@ fun x -> (%l' - x) |?| r.Distribution.Support @>), <@ fun x -> %l' + x @>, Scalar l' + r.Expectation)
    static member (+) (l:ContinuousRandomVariable, r:'a) = let r' = realexpr r in l.Transform((l.Distribution.Support |>| <@ fun x -> (x + %r') |?| l.Distribution.Support @>), <@ fun x -> x + %r' @>, Scalar r' + l.Expectation)

    static member (*) (l:'a, r:ContinuousRandomVariable) = let l' = realexpr l in r.Transform((r.Distribution.Support |>| <@ fun x -> (%l' * x) |?| r.Distribution.Support @>), <@ fun x -> %l' * x @>, Scalar l' * r.Expectation)
    static member (*) (l:ContinuousRandomVariable, r:'a) = let r' = realexpr r in l.Transform((l.Distribution.Support |>| <@ fun x -> (x * %r') |?| l.Distribution.Support @>), <@ fun x -> x * %r' @>, Scalar r' * l.Expectation)

    static member (/) (l:'a, r:ContinuousRandomVariable) = let l' = realexpr l in r.Transform( (r.Distribution.Support |>| <@ fun x -> (%l' / x) |?| r.Distribution.Support @>), <@ fun x -> %l' / x @>, Scalar l' / r.Expectation)
    static member (/) (l:ContinuousRandomVariable, r:'a) = let r' = realexpr r in l.Transform((l.Distribution.Support |>| <@ fun x -> (x / %r') |?| l.Distribution.Support @>), <@ fun x -> x / %r' @>, Scalar r' / l.Expectation)
   
[<AutoOpen>]
module ProbabilityDistribution =
    let discrete_distr (s:ISet<real>) (pmf:Expr<real->real>) = DiscreteProbabilityDistribution(s, pmf) 
    
    let drandvar (distr:DiscreteProbabilityDistribution) = DiscreteRandomVariable distr

    let drandvar_m distr m = DiscreteRandomVariable(distr, m)

    let continuous_distr s d = ContinuousProbabilityDistribution(s, d)

    let crandvar (distr:ContinuousProbabilityDistribution) = ContinuousRandomVariable distr

    let crandvar_m distr m = ContinuousRandomVariable(distr, m)

    let degenerate a = discrete_distr (finite_seq [a]) <@ fun x -> 1. @>

    let uniform s = let l = (Seq.length s) in discrete_distr (finite_seq s)  <@ fun x -> 1. / real l  @>
    
    let poisson l = discrete_distr (infinite_seq <@ fun i -> real i @> (fun r -> is_int r))  <@ fun x -> l ** x * (Math.e ** - l) / (factorial ((int) x)) @> 

    //let poisson_var l = discrete_var_m (poisson l) (realterm l)

    let binomial p n = discrete_distr ([0. .. real n] |> finite_seq)  <@ fun x -> binomial_coeff n ((int) x) * ((p ** x) * ((1.-p) ** (real n - x))) @> 
    
    let t = poisson 5.

    let r = t.[5.]
    let bernoulli<'t when 't : equality> p = binomial p 1

    let geometric<'t when 't : equality> p n = discrete_distr ([0. .. real n] |> finite_seq)  <@ fun x -> ((1.-p) ** (x - 1.)) * p @>
    
    let uniform_continuous<'t when 't : equality> a b = 
        let a',b' = realexpr a, realexpr b
        continuous_distr (open_interval a b) <@ fun x -> 1. / (%b' - %a') @>

    let std_normal x = continuous_distr (open_interval minf inf) <@ fun z -> Math.e ** ((-z**2.) / 2.) @>