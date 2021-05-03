namespace Sylvester

open Sylvester.Arithmetic
open Vector

type R<'n when 'n :>Number>() = 
    inherit VectorSpace<'n, real, Vector<'n, real>>(Field.R, Vector.vadd, Vector.vsmul)
    
type Region<'n when 'n :> Number> = Set<Vec<'n>>

[<AutoOpen>]
module R =
    let R<'n when 'n :> Number> =  R<'n>()
    
    [<Formula>]
    let open_interval left right = Field.R |>| (fun x -> x > left && x < right)
    
    [<Formula>]
    let closed_interval left right = Field.R |>| (fun x -> x >= left && x <= right)
    
    [<Formula>]
    let open_ball (x:Vec<_>) (r:real) = R |>| (fun y -> (vdist x y) < Scalar r)