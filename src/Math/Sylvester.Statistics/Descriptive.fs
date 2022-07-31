namespace Sylvester.Statistics

open MathNet.Numerics.Statistics

open Sylvester

module Descriptive =
    
    let mean (s:seq<'t>) = s |> real_seq |> Statistics.Mean |> real

    let median (s:seq<'t>) = s |> real_seq |> Statistics.Median |> real

    let inter_quartile_range (s:seq<'t>) = s |> real_seq |> Statistics.InterquartileRange |> real