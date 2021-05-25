namespace Sylvester

open FSharp.Quotations

type IRandomElement<'t, 'd when 't : equality> = 
    abstract Distribution : ProbabilityDistribution<'t> option

and ProbabilityDistribution<'t when 't:equality> = 
| ProbabilityMass of Expr<int -> real>
| ProbabilityDensity of Expr<real -> real>
    
type RandomVariable<'t when 't : equality>(support: Set<real> option, map:Expr<'t -> real> option, distr:ProbabilityDistribution<'t> option) = 
    member val Support = defaultArg support (setOf Field.R)
    member val Map = map 
    member val Map' = Option.bind(evaluate >> Option.Some) map
    member val Distribution = distr
    interface IRandomElement<'t, real> with
        member x.Distribution = distr
    static member DefaultSupport = setOf Field.R

type Discrete<'t when 't : equality>(?support:Set<real>, ?map:Expr<'t -> real>, ?d:Expr<int->real>) = 
    inherit RandomVariable<'t>(support, map, d |> Option.bind (ProbabilityMass >> Some))
    member x.Prob(n:int) = 
        match x.Distribution with
        | Some(ProbabilityMass m) -> evaluate m <| n
        | _ -> failwith "The distribution function for this random variable is not defined."
type Continuous<'t when 't : equality>(?support:Set<real>, ?map:Expr<'t->real>, ?d:Expr<real->real>) = 
    inherit RandomVariable<'t>(support, map, d |> Option.bind (integrate >> ProbabilityDensity >> Some))
    member x.Prob(n:real) = 
        match x.Distribution with
        | Some(ProbabilityDensity d) -> evaluate d <| n
        | _ -> failwith "The distribution function for this random variable is not defined."
   
[<AutoOpen>]
module ProbabilityDistribution =
    let random() = RandomVariable(None,None,None)

    let discrete distr = Discrete(d = distr)

    let continuous distr = Continuous(d = distr)

    let poisson l = discrete <@ fun n -> l ** real n * Math.e ** -l / real (Math.factorial n) @>