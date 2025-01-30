namespace Sylvester

open FSharp.Quotations

open Arithmetic

type IUnivariateDistribution =
    abstract member Support: Set<real>
    abstract member Func: RealFunction
    abstract member Expectation: Scalar<real>
    abstract member ProbMap: Expr<real->real>
    abstract member Prob: a:int->Scalar<real>
    abstract member Prob: a:real->Scalar<real>
    abstract member Prob: a:Scalar<real>->Scalar<real>
    abstract member CProb: a:real->Scalar<real>
    abstract member CProb: a:int*int->Scalar<real>
    abstract member CProb: a:real*real->Scalar<real>
    abstract member CProb: a:Scalar<real>*Scalar<real>->Scalar<real>
    abstract member Transform: support:ISet<real>*t:Expr<real->real>-> IUnivariateDistribution

type DiscreteProbabilityDistribution(support:ISet<real>, pmf:Expr<real->real>, ?mean:Scalar<real>, ?probmap:Expr<real->real>) = 
    member x.Support = support.Set 
    member x.Pmf = realfun_l pmf
    member x.Mean = defaultArg mean (support.Set |> Seq.map(fun e ->  e * (x.Prob e)) |> Seq.reduce (+))
    member x.ProbMap = defaultArg probmap pmf |> ev
    member x.Prob(a:int) = if real a |?| x.Support then x.ProbMap (real a) |> Scalar else 0R
    member x.Prob(a:real) = if a |?| x.Support then x.ProbMap a |> Scalar else 0R
    member x.Prob(a:Scalar<real>) = x.Pmf.[a]
    member x.CProb(a:int, b:int) = [a .. b] |> Seq.map x.Prob |> Seq.reduce (+) |> simplify
    member x.CProb(a:real, b:real) = [a .. b] |> Seq.map x.Prob |> Seq.reduce (+) |> simplify
    member x.CProb(a:Scalar<real>, b:Scalar<real>) = failwith "not supported"
    member x.Transform(support:ISet<real>, t:Expr<real->real>, ?probmap:Expr<real->real>) = 
        let vd = param_var x.Pmf.MapExpr
        let vt = param_var t
        let bodyt = t |> body |> subst_var_value vt (Expr.Var vd)
        let td = x.Pmf.MapExpr |> body |> subst_var_value vd bodyt |> recombine_func_as<real->real> [vd] |> realfun_l
        if probmap.IsSome then
            let vd = param_var probmap.Value
            let vt = param_var t
            let bodyt = t |> body |> subst_var_value vt (Expr.Var vd)
            let ptd = probmap.Value |> body |> subst_var_value vd bodyt |> recombine_func_as<real->real> [vd] 
            DiscreteProbabilityDistribution(support, td.MapExpr, td.[x.Mean], ptd)
        else
             DiscreteProbabilityDistribution(support, td.MapExpr, td.[x.Mean])
    member x.Item(a:int) = x.Prob a
    member x.Item(a:real) = x.Prob a
    member x.Item(a:Scalar<real>) = x.Prob a
    member x.Item(a: int, b:int) = x.CProb(a, b)
    member x.Item(a: real, b:real) = x.CProb(a, b)
    
    interface IUnivariateDistribution with
        member x.Support = x.Support
        member x.Func = x.Pmf
        member x.Expectation = x.Mean
        member x.ProbMap = defaultArg probmap pmf
        member x.Prob(a:int) = x.Prob a
        member x.Prob(a:real) = x.Prob a
        member x.Prob(a:Scalar<real>) = x.Prob a
        member x.CProb(a:real) = x.CProb(0.,a)
        member x.CProb(a:int, b:int) = x.CProb(a,b)
        member x.CProb(a:real,b:real) = x.CProb(a,b)
        member x.CProb(a:Scalar<real>,b:Scalar<real>) : Scalar<real> = x.CProb(a,b)
        member x.Transform(support:ISet<real>, t:Expr<real->real>) = x.Transform(support, t) :> IUnivariateDistribution
       
type ContinuousProbabilityDistribution(support:ISet<real>, pdf:Expr<real->real>, ?mean:Scalar<real>, ?cdf:Expr<real->real>) = 
    member x.Support = support.Set 
    member x.Pdf = realfun_l pdf
    member x.Cdf = 
        if cdf.IsNone then 
            let v = param_var pdf
            let b = pdf |> body |> expand_as<real> |> Ops.DefiniteIntegral (exprvar<real> v) (exprv minf) (exprvar<real> v)
            let f = recombine_func_as<real->real> [v] b
            f 
        else cdf.Value
    member x.CProbMap = ev x.Cdf 
    member x.Mean =  if mean.IsNone then let v = x.Pdf.ScalarVar in integrate_fun_over_R (v * x.Pdf) else mean.Value
    member x.Prob(a:int) = 0R
    member x.Prob(a:real) = 0R
    member x.Prob(a:Scalar<real>) = 0R
    member x.CProb(a:int) = x.CProbMap(real a) |> Scalar 
    member x.CProb(a:real) =  x.CProbMap(a) |> Scalar 
    member x.CProb(a:Scalar<real>) = 
        let v = param_var x.Cdf in
        x.Cdf |> body |> subst_var_value v a.Expr |> expand_as<real> |> Scalar
    member x.CProb(a:int, b:int) = x.CProb b - x.CProb a
    member x.CProb(a:real, b:real) = x.CProb b - x.CProb a
    member x.CProb(a:Scalar<real>, b:Scalar<real>) = x.CProb b - x.CProb a
    member x.Transform(support:ISet<real>, t:Expr<real->real>) = 
       let vd = param_var x.Pdf.MapExpr
       let vt = param_var t
       let bodyt = t |> body |> subst_var_value vt (Expr.Var vd)
       let td = x.Pdf.MapExpr |> body |> subst_var_value vd bodyt |> recombine_func_as<real->real> [vd] |> realfun_l
       
       let vd2 = param_var x.Cdf
       let bodyt2 = t |> body |> subst_var_value vt (Expr.Var vd2)
       let ctd = x.Cdf |> body |> subst_var_value vd2 bodyt2 |> recombine_func_as<real->real> [vd2] 
       ContinuousProbabilityDistribution(support, td.MapExpr, Scalar <@ (%t)(%(x.Mean.Expr)) @>, ctd)
       
    member x.Item(a:int) = x.CProb(a)
    member x.Item(a:real) = x.CProb(a)
    member x.Item(a:int, b:int) = x.CProb(a, b)
    member x.Item(a:real, b:real) = x.CProb(a, b)
    member x.Item(a:Scalar<real>, b:Scalar<real>) = x.CProb(a, b)

    interface IUnivariateDistribution with
        member x.Support = x.Support
        member x.Func = x.Pdf
        member x.Expectation = x.Mean
        member x.ProbMap = x.Cdf
        member x.Prob(a:int) = 0R
        member x.Prob(a:real) = 0R
        member x.Prob(a:Scalar<real>) = 0R
        member x.CProb(a:real) = a |> x.CProbMap |> Scalar<real>
        member x.CProb(a:int, b:int) = x.CProb(a, b)
        member x.CProb(a:real, b:real) = x.CProb(a, b)
        member x.CProb(a:Scalar<real>,b:Scalar<real>) : Scalar<real> = x.CProb(a,b)
        member x.Transform(support:ISet<real>, t:Expr<real->real>) = x.Transform(support, t) :> IUnivariateDistribution

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
    
type RandomVariable<'d when 'd  :> IUnivariateDistribution>(distr:'d) = 
    member val Distribution = distr 
    member val Expectation = distr.Expectation
    member x.Transform(s, t) = RandomVariable(distr.Transform(s,t))
    member x.Prob(a:int) = a |> real |> x.Distribution.Prob
    member x.Prob(a:real) = x.Distribution.Prob a
    member x.Prob(a:Scalar<real>) = x.Distribution.Prob a
    member x.CProb(a:int, b:int) = x.Distribution.CProb(real a, real b)
    member x.CProb(a:real, b:real) = x.Distribution.CProb(a, b)
    member x.CProb(a:Scalar<real>,b:Scalar<real>) : Scalar<real> = x.Distribution.CProb(a,b)
    member x.Item(a:int) = if box x.Distribution :? DiscreteProbabilityDistribution then x.Distribution.Prob a else a |> real |> x.Distribution.CProb 
    member x.Item(a:real) = if box x.Distribution :? DiscreteProbabilityDistribution then x.Distribution.Prob a else x.Distribution.CProb a 
    member x.Item(a:Scalar<real>) = x.Prob(a)
    member x.Item(a:int, b:int) = x.CProb(a, b)
    member x.Item(a:real, b:real) = x.CProb(a, b)
    member x.Item(a:Scalar<real>, b:Scalar<real>) = x.CProb(a, b)
    
    static member (-) (l:'a, r:RandomVariable<'d>) = let l' = realexpr l in r.Transform((r.Distribution.Support |>| <@ fun x -> (%l' - x) |?| r.Distribution.Support @>), <@ fun x -> %l' - x @>) 
    static member (-) (l:RandomVariable<'d>, r:'a) = let r' = realexpr r in l.Transform((l.Distribution.Support |>| <@ fun x -> (x - %r') |?| l.Distribution.Support @>), <@ fun x -> x - %r' @>)

    static member (+) (l:'a, r:RandomVariable<'d>) = let l' = realexpr l in r.Transform((r.Distribution.Support |>| <@ fun x -> (%l' - x) |?| r.Distribution.Support @>), <@ fun x -> %l' + x @>)
    static member (+) (l:RandomVariable<'d>, r:'a) = let r' = realexpr r in l.Transform((l.Distribution.Support |>| <@ fun x -> (x + %r') |?| l.Distribution.Support @>), <@ fun x -> x + %r' @>)

    static member (*) (l:'a, r:RandomVariable<'d>) = let l' = realexpr l in r.Transform((r.Distribution.Support |>| <@ fun x -> (%l' * x) |?| r.Distribution.Support @>), <@ fun x -> %l' * x @>)
    static member (*) (l:RandomVariable<'d>, r:'a) = let r' = realexpr r in l.Transform((l.Distribution.Support |>| <@ fun x -> (x * %r') |?| l.Distribution.Support @>), <@ fun x -> x * %r' @>)

    static member (/) (l:'a, r:RandomVariable<'d>) = let l' = realexpr l in r.Transform( (r.Distribution.Support |>| <@ fun x -> (%l' / x) |?| r.Distribution.Support @>), <@ fun x -> %l' / x @>)
    static member (/) (l:RandomVariable<'d>, r:'a) = let r' = realexpr r in l.Transform((l.Distribution.Support |>| <@ fun x -> (x / %r') |?| l.Distribution.Support @>), <@ fun x -> x / %r' @>)
   
type DegenerateDistribution(a) = 
    inherit DiscreteProbabilityDistribution(finite_seq [a], <@ fun x -> 1. @>)
   
type DiscreteUniformDistribution(s:seq<real>) = 
    inherit DiscreteProbabilityDistribution(finite_seq s,  let l = (Seq.length s) in <@ fun x -> 1. / real l  @>)
  
type PoissonDistribution(l:real) = 
    inherit DiscreteProbabilityDistribution((infinite_seq <@ real @> (fun r -> is_int r)) , <@ fun x -> l ** x * (Math.e ** - l) / (factorial ((int) x)) @>, Scalar l)
   
type BinomialDistribution(p:real, n:int) = 
    inherit DiscreteProbabilityDistribution(finite_seq [0. .. real n], <@ fun x -> binomial_coeff n ((int) x) * ((p ** x) * ((1.- p) ** (real n - x))) @>)

type BernoulliDistribution(p:real) = inherit BinomialDistribution(p, 1) 

type GeometricDistribution(p:real, n:int) = 
    inherit DiscreteProbabilityDistribution(finite_seq [0. .. real n], <@ fun x -> ((1.-p) ** (x - 1.)) * p @>)

type ContinuousUniformDistribution(a:real, b:real) =
    inherit ContinuousProbabilityDistribution((open_interval a b), <@ fun x -> 1. / (%(realexpr b) - %(realexpr a)) @>, Scalar<real>((a + b) / 2.), <@ fun z ->  if z  >= a && z <= b then (z - a) / (b - a) else 0. @>) 

type NormalDistribution(m:real, sd:real) =
    inherit ContinuousProbabilityDistribution(Field.R, <@ fun z -> (Math.e ** (((-(z - m) / 2. * sd ** 2.)**2.)) / sqrt(2. * Math.pi * (sd ** 2.))) @>,
                Scalar m, <@ fun x -> 0.5 * (1. + erf ((x - m) / (sd * sqrt 2. ))) @>) 

[<AutoOpen>]
module ProbabilityDistribution =
    let discrete_distr (s:ISet<real>) (pmf:Expr<real->real>) = DiscreteProbabilityDistribution(s, pmf) 
    
    let continuous_distr s d c m = ContinuousProbabilityDistribution(s, d, ?cdf=c, ?mean=m)

    let randvar<'d when 'd :> IUnivariateDistribution> (distr:'d) = RandomVariable distr

    let degenerate a = discrete_distr (finite_seq [a]) <@ fun x -> 1. @>

    let uniform s = let l = (Seq.length s) in discrete_distr (finite_seq s)  <@ fun x -> 1. / real l  @>
    
    let poisson l = discrete_distr (infinite_seq <@ fun i -> real i @> (fun r -> is_int r))  <@ fun x -> l ** x * (Math.e ** - l) / (factorial ((int) x)) @> 

    let binomial p n = discrete_distr ([0. .. real n] |> finite_seq)  <@ fun x -> binomial_coeff n ((int) x) * ((p ** x) * ((1.-p) ** (real n - x))) @> 
    
    let bernoulli<'t when 't : equality> p = binomial p 1

    let geometric<'t when 't : equality> p n = discrete_distr ([0. .. real n] |> finite_seq)  <@ fun x -> ((1.-p) ** (x - 1.)) * p @>
    
    let continuous_uniform<'t when 't : equality> a b = ContinuousUniformDistribution(a, b)
    
    let normal m sd = NormalDistribution(m, sd)

    let normalvar m sd = normal m sd |> randvar