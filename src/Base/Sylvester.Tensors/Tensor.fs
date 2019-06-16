namespace Sylvester.Tensors

open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10
open Sylvester.Arithmetic.Collections


type Tensor<'n, 't when 'n: (static member Zero : N0) 
                    and 'n : (static member op_Explicit: 'n -> int) 
                    and 't : (static member Zero: 't) 
                    and 't : (static member (+): 't -> 't -> 't)> () =     
    
    //abstract member x.Dims: HList<'n>
    member inline x.Rank = getN<'n>()

    





    




