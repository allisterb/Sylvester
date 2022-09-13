namespace Sylvester

open MathNet.Numerics.Statistics
open MathNet.Numerics.Distributions
open MathNet.Numerics.Random

open Sylvester

module DescriptiveStatistics =
    
    let mean (s:seq<'t>) = s |> real_seq |> Statistics.Mean |> real

    let median (s:seq<'t>) = s |> real_seq |> Statistics.Median |> real

    let inter_quartile_range (s:seq<'t>) = s |> real_seq |> Statistics.InterquartileRange |> real

    let normal_distrib mean variance = new Normal(mean, variance)

    let std_normal_distrib = new Normal()

    let cumul_prob (d:IUnivariateDistribution) x : real = d.CumulativeDistribution x

    let phi (x:real) : real  = cumul_prob std_normal_distrib x

    let interval_prob (d:IUnivariateDistribution) (a:real) (b:real) =
       if b < a then failwith "The right of the interval must be greater than the left."
       cumul_prob d b - cumul_prob d a

    let phi_interval a b = interval_prob std_normal_distrib a b