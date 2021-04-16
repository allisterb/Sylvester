namespace Sylvester

module Series =
    [<Formula>]
    let geometric_series a r = infinite_series (fun n -> a * r ** (real n - 1.))
    
    let geometric_series' = infinite_series' <@ fun n r -> r ** (real n - 1.) @>

    [<Formula>]
    let harmonic_series = infinite_series (fun n -> 1Q / n)

    let harmonic_series' = infinite_series' <@ fun n r -> 1. / r ** (real n - 1.) @>