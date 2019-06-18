namespace Sylvester.Tensors

open System.Collections.Generic
open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10
open Sylvester.Arithmetic.Collections
open Sylvester.Arithmetic.Collections

type Matrix<'r, 'c, 't when 'r: (static member Zero : N0) 
                        and 'c: (static member Zero : N0)
                        and 'r : (static member op_Explicit: 'r  -> int)
                        and 'c : (static member op_Explicit: 'c  -> int) 
                        and 't : (static member Zero: 't) and 't : (static member (+): 't -> 't -> 't)>() = 
    inherit Tensor<NTwo, 't>()

    member inline x.Dims = vanew<'c, VArray<'r, 't>>(vanew'<'r, 't>) ^+^ VNil |> varrays
   
    member inline x.Item(a:'a when 'a : (static member (+<): 'a -> 'c -> True)) :VArray<'r, 't> = 
        let r = x.Dims |@| zero in r.[a] 

    static member inline (*) (l:Matrix<'a, 'b, 't>, r:Matrix<'b, 'c, 't>) = new Matrix<'b, 'c, 't>()

    static member inline (+) (l:Matrix<'a, 'b, 't>, r:Matrix<'a, 'b, 't>) = new Matrix<'a, 'b, 't>() 
