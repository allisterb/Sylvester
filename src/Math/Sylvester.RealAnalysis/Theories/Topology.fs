namespace Sylvester

open Sylvester.Arithmetic

module Topology =
    let gg<'n when 'n :> Number> = term<R<'n>>

    let zz = gg<dim<3>> "s"
    let x = zz.[4]

