namespace Sylvester.Tensors

open System.Collections.Generic

open FsAlg

open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10
open Sylvester.Arithmetic.Collections
open Sylvester.Arithmetic.Collections

type Matrix<'r, 'c, 't when 'r: (static member Zero : N0) 
                        and 'c: (static member Zero : N0)
                        and 'r : (static member op_Explicit: 'r  -> int)
                        and 'c : (static member op_Explicit: 'c  -> int) 
                        and 't : (static member Zero: 't)  
                        and 't : (static member One : 't)
                        and 't : (static member (+) : 't * 't -> 't)
                        and 't : (static member (-) : 't * 't -> 't)
                        and 't : (static member (*) : 't * 't -> 't)
                        and 't : (static member (/) : 't * 't -> 't)
                        and 't : (static member (~-) : 't -> 't)
                        and 't : (static member Abs : 't -> 't)
                        and 't : (static member Pow : 't * 't -> 't)
                        and 't : (static member Sqrt : 't -> 't)
                        and 't : (static member op_Explicit : 't -> float)
                        and 't : comparison>() = 
    inherit Tensor<NTwo, 't>()

    member inline x.Dims = va2dnew'<'r, 'c, 't> ^+^ VNil |> varrays

    member inline x.Array = x.Dims |@| zero 
   
    member inline x._Array = let a = x.Dims |@| zero in a._Array
    
    member inline x.SetVal(i:'i, j:'j, item: 't) = x.Array.SetVal(i, j, item)
    
    member inline x.Item(i:'i, j:'j) = x.Array.[i, j]

    member inline x.Item(i:int, j:int) = x._Array.[i, j]

    static member inline (*) (l:Matrix<'i, 'j, 't>, r:Matrix<'j, 'k, 't>) = 
        let m = Matrix<'i, 'k, 't>() in m.Array |> va2dinit ((Generic.Matrix(l._Array) * Generic.Matrix(r._Array)).ToArray2D()) |> ignore
        m

    static member inline (+) (l:Matrix<'a, 'b, 't>, r:Matrix<'a, 'b, 't>) =
        let m = Matrix<'a, 'b, 't>() in m.Array |> va2dinit ((Generic.Matrix(l._Array) + Generic.Matrix(r._Array)).ToArray2D()) |> ignore
        m
        

        
        
        
