namespace Sylvester.Tensors

open System.Collections.Generic
open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10
open Sylvester.Arithmetic.Collections

type Vector<'n, 't when 'n: (static member Zero : N0) and 'n : (static member op_Explicit: 'n -> int) 
                    and 't : (static member Zero: 't) and 't : (static member (+): 't -> 't -> 't)>() = 
    inherit Tensor<N1<``1``>, 't>()

    member inline x.Length = getN<'n>()
    
    member inline x.IntLength = x.Length |> int
    
    member inline x.Dims = (vlist  x.Rank [new List<'t>(x.IntLength)]) ^+^ VNil

    member inline x.SetVal(v:VList<'n, 't>) = ()