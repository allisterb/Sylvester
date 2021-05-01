namespace Sylvester

open System
open FSharp.Quotations

module Series =
    [<Formula>]
    let geometric_series (a:real) r = infinite_series (fun n -> a * r ** (real n - 1.))
    
    let geometric_series' (a:Expr<real>) = infinite_series' <@ fun n r -> %a * r ** (real n - 1.) @>

    [<Formula>]
    let harmonic_series = infinite_series (fun n -> 1Q / n)

    let harmonic_series' = infinite_series' <@ fun n _ -> 1 / n @>