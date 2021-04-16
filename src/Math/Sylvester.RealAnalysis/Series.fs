namespace Sylvester

module Series =
    [<Formula>]
    let geometric_series_fn a r n = a * r ** (real n - 1.)

    [<Formula>]
    let geometric_series a r = infinite_series (geometric_series_fn a r)
    
    [<Formula>]
    let harmonic_series = infinite_series (fun n -> 1. / real n)