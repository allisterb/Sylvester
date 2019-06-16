namespace Sylvester.Tensors

open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10
open Sylvester.Arithmetic.Collections

type Vector<'n, 't when 'n: (static member Zero : N0) and 'n : (static member op_Explicit: 'n -> int) 
                    and 't : (static member Zero: 't) and 't : (static member (+): 't -> 't -> 't)>() = 
    inherit Tensor<N1<``1``>, 't>()

