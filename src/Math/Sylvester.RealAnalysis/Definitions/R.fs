namespace Sylvester

open Sylvester.Arithmetic

type R<'n when 'n :>Number>() = inherit VectorSpace<'n, real, Vector<'n, real>>(Field.R, Vector.vadd, Vector.vsmul)
 
[<AutoOpen>]
module R =
    let R<'n when 'n :> Number> =  R<'n>()
    let open_interval left right = Field.R |>| (fun x -> x > left && x < right)
    let closed_interval left right = Field.R |>| (fun x -> x >= left && x <= right)
    let line (origin:real) (step:real) = infinite_seq (fun n -> origin + ((real n) * step)) 
    let axis step = line 0.0 step
