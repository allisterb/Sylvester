namespace Sylvester.Tensors

open Sylvester.Arithmetic.N10
open Sylvester.Arithmetic.Collections

type Scalar<'t when 't : (static member Zero: 't) and 't : (static member (+): 't -> 't -> 't)>() = 
    inherit Tensor<N0, 't>()
