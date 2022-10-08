namespace Sylvester

open MathNet.Numerics.Statistics
open MathNet.Numerics.Distributions
open MathNet.Numerics.Random

open Sylvester

module DescriptiveStatistics =
    let mean (s:seq<'t>) = s |> real_seq |> Statistics.Mean |> real

    let median (s:seq<'t>) = s |> real_seq |> Statistics.Median |> real

    let standard_deviation (s:seq<'t>) = s |> real_seq |> Statistics.StandardDeviation |> real

    let inter_quartile_range (s:seq<'t>) = s |> real_seq |> Statistics.InterquartileRange |> real

    let prob_cumul (d:IUnivariateDistribution) (x:real) : real = d.CumulativeDistribution x

    let prob_interval (d:IUnivariateDistribution) (a:real) (b:real) =
       if b < a then failwith "The right of the interval must be greater than the left."
       prob_cumul d b - prob_cumul d a

    let normal_distrib (mean:real) (variance:real) = new Normal(mean, (sqrt variance))

    let t_distrib mean variance d = new StudentT(mean, (sqrt variance), d)

    let std_t_distrib (n:int) = new StudentT(0., 1.0, real n)

    let std_normal_distrib = new Normal()

    let chi_sq f  = new ChiSquared(f)

    let phi (x:real) : real  = prob_cumul std_normal_distrib x

    let phi_interval a b = prob_interval std_normal_distrib a b

    let inverse_phi (x:real) :real= std_normal_distrib.InverseCumulativeDistribution x

    let inverse_std_t (n:int) (p:real) :real = abs <| (std_t_distrib n).InverseCumulativeDistribution p

    let sampling_distrib mean variance n = normal_distrib mean (variance / n)

    let z_alpha a = inverse_phi (1. - (a / 2.))

    let t_alpha n a = inverse_std_t n (1. - (a / 2.))
    
    let estimate_pop_mean_pop_sd_interval (confidence:int) (n:int) pop_sd sample_mean = 
        let z = z_alpha (1. - (real confidence / 100.)) in
        (z * pop_sd / sqrt (real n)), sample_mean - (z * pop_sd / sqrt (real n)), sample_mean + (z * pop_sd / sqrt (real n)) 

    let estimate_pop_mean_sample_sd_interval (confidence:int) (n:int) sample_sd sample_mean = 
        let z = t_alpha (n - 1) (1. - (real confidence / 100.)) in
        (z * sample_sd / sqrt (real n)), sample_mean - (z * sample_sd / sqrt (real n)), sample_mean + (z * sample_sd / sqrt (real n))

    let estimate_pop_mean_interval (confidence:int) (s:seq<'t>) =
        let sample_mean = mean s
        let sample_sd = standard_deviation s
        estimate_pop_mean_sample_sd_interval confidence (Seq.length s) sample_sd sample_mean

    let proportion_sampling_distrib prop (n:int) = normal_distrib prop (prop * (1. - prop) / real n)