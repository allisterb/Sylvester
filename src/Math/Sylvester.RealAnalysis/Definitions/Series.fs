namespace Sylvester

open System

module Series =
    [<Formula>]
    let abs (r:real) :real= Math.Abs r

    [<Formula>]
    let geometric_series a r = infinite_series (fun n -> a * r ** (real n - 1.))
    
    let geometric_series' = infinite_series' <@ fun n r -> r ** (real n - 1.) @>

    [<Formula>]
    let harmonic_series = infinite_series (fun n -> 1Q / n)

    let harmonic_series' = infinite_series' <@ fun n r -> Rational.One / r ** (n - 1) @>