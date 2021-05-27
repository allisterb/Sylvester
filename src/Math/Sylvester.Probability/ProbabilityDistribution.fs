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

type IRandomElement<'t, 'd when 't : equality> = interface end

type RandomVariable<'t when 't : equality>(map:Expr<'t -> real> option, support: Set<real> option, distr:ProbabilityDistribution<'t> option) = 
    member val Map = map 
    member val Map' = Option.bind(evaluate >> Option.Some) map
    member val Support = defaultArg support RandomVariable<'t>.DefaultSupport
    member val Distribution = defaultArg distr (ProbabilityMass(<@ fun _ -> 0. @>))
    member x.Func =
        fun (a:real) ->
            let v = param_var x.Distribution.Expr
            x.Distribution.Expr |> body |> subst_var_value v (Expr.Value a) |> expand''<real> |> Scalar
    member x.Prob = fun a -> if a |?| x.Support then x.Func a else 0R
    member x.ProbExpr = 
        fun (a:Expr<real>) ->
            let v = param_var x.Distribution.Expr
            let b = x.Distribution.Expr |> body |> subst_var_value v a 
            <@ %%b:real @> |> Scalar<real>
    interface IRandomElement<'t, real>
    static member DefaultSupport = setOf Field.R
    interface ISet<real> with 
        member x.Set = x.Support
        member x.Equals b = x.Support.Equals b

type Discrete<'t when 't : equality>(?map:Expr<'t -> real>, ?support:Set<real>, ?pmf:Expr<real->real>) = 
    inherit RandomVariable<'t>(map, support, pmf |> Option.bind (ProbabilityMass >> Some))
    member x.Cdf = fun i -> seq {0. .. i} |> Seq.map x.Prob |> Seq.reduce (+) 
    member x.Expectation =
        let p = let b = body x.Distribution.Expr in <@ %%b:real @>
        let n = param_var_expr x.Distribution.Expr
        sum(<@ %p * %n @>) n (Seq.min x.Support) (Seq.max x.Support)
    static member (-) (l:real, r:Discrete<'t>) =
        let p =            
            let v = param_var r.Distribution.Expr
            r.Distribution.Expr |> body |> subst_var_value v (call_sub (Expr.Value l) (Expr.Var v)) |> recombine_func [v] 
        Discrete<'t>(support = r.Support, pmf = <@ %%p:real->real @>)

type Continuous<'t when 't : equality>(?map:Expr<'t->real>, ?support:Set<real>, ?pdf:Expr<real->real>) = 
    inherit RandomVariable<'t>(map, support, pdf |> Option.bind (integrate >> ProbabilityDensity >> Some))
   
[<AutoOpen>]
module ProbabilityDistribution =
    let discrete<'t when 't : equality> s d = Discrete<'t>(support=s, pmf=d)

    let continuous<'t when 't : equality> d = Continuous<'t>(pdf=d)

    let degenerate<'t when 't : equality> a = discrete<'t> (finite_seq [a]) <@ fun x -> 1. @>

    let uniform<'t when 't : equality> s = let l = (Seq.length s) in discrete<'t> (finite_seq s) <@ fun x -> 1. / real l  @>
    
    let poisson<'t when 't : equality> l n = discrete<'t> ([0. .. (real n)] |> finite_seq) <@ fun x -> l ** x * (Math.e ** -l) / (factorial ((int) x)) @>

    let binomial<'t when 't : equality> p n = discrete<'t> ([0. .. real n] |> finite_seq) <@ fun x -> binomial_coeff n ((int) x) * ((p ** x) * ((1.-p) ** (real n - x))) @> 
    
    let bernoulli<'t when 't : equality> p = binomial p 1