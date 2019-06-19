namespace Sylvester.Arithmetic.Collections
 
open System.Collections.Generic
open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10
open System

type VArray2D<'d0, 'd1, 't when 'd0: (static member Zero : N0) and 'd0 : (static member op_Explicit: 'd0 -> int)
                            and 'd1: (static member Zero : N0) and 'd1 : (static member op_Explicit: 'd1 -> int)> () =
                            
    static member inline VArray = _true

    static member inline (!+) = (getN<'d0>(), getN<'d1>())

    static member inline (^+^) (x:VArray2D<'xd0, 'xd1, 't>, y: VArray2D<'yd0, 'yd1, 't>) = x ^+^ y ^+^ VNil
    
    member inline x.Length0 = getN<'d0>() |> int

    member inline x.Length1 = getN<'d1>() |> int

    member inline x.IntLength0 = x.Length0 |> int

    member inline x.IntLength1 = x.Length1 |> int
   
    member inline x._Array = Array2D.create x.IntLength0 x.IntLength1 Unchecked.defaultof<'t>

    member inline x.SetVal(i:'i, j:'j, item: 't  when 'i : (static member (+<): 'i -> 'd0 -> True) 
                                                   and 'j : (static member (+<): 'j -> 'd1 -> True))  = 
        x._Array.[i |> int, j |> int] <- item

    member inline x.SetVal(i:int, j:int, item:'t) =
        if i < (x.IntLength0) && j < x.IntLength1 then  x._Array.[i, j] <- item else raise(IndexOutOfRangeException("i,j"))

    member inline x.Item(i:'i, j:'j when 'i : (static member (+<): 'i -> 'd0 -> True) 
                                     and 'j : (static member (+<): 'j -> 'd1 -> True)) : 't = 
        x._Array.[i |> int, j |> int]
        
    member inline x.GetSlice(start0: 'start0 option, finish0: 'finish0 option, start1: 'start1 option, finish1: 'finish1 option 
                            when ('start0 or 'finish0 or 'start1 or 'finish1): (static member Zero : N0) 
                            and 'start0 : (static member op_Explicit: 'start0 -> int) 
                            and 'finish0 : (static member op_Explicit: 'finish0 -> int)
                            and 'start1 : (static member op_Explicit: 'start1 -> int) 
                            and 'finish1 : (static member op_Explicit: 'finish1 -> int)
                            and 'start0 : (static member (+<): 'start0 -> 'd0 -> True)
                            and 'finish0 : (static member (+<): 'finish0 -> 'd0 -> True)
                            and 'start1 : (static member (+<): 'start1 -> 'd1 -> True)
                            and 'finish1 : (static member (+<): 'finish1 -> 'd1 -> True)

                            and 'b : (static member (+<): 'b -> 'n -> True)) : VArray2D<'c0, 'c1, 't> = 
                                       
        let inline create(z0:'z0, z1:'z1, items:'t[,] when ('z0 or 'z1): (static member Zero : N0) 
                                         and 'z0 : (static member op_Explicit: 'z0 -> int)
                                         and 'z1 : (static member op_Explicit: 'z1 -> int)) =
            let v = VArray2D<'z0, 'z1,'t>()
            for i in 0..v.Length0 - 1 do 
                for j in 0 ..v.Length1 do
                    v.SetVal(i, j, items.[i, j])
            v

        let _start0, _finish0 = start0.Value, finish0.Value
        let _start1, _finish1 = start1.Value, finish1.Value
        let intstart0, intfinish0 = _start0 |> int, _finish0 |> int
        let intstart1, intfinish1 = _start1 |> int, _finish1 |> int
        let length0, length1 = _finish0 - _start0, _finish1 - _start1                                                       
        create(length0, length1, x._Array.[intstart0..intfinish0, intstart1..intfinish1])
        
        
                                        
    member inline x.At<'i, 'j when  'i : (static member Zero : N0) and 'j : (static member Zero : N0) 
                            and 'i : (static member (+<): 'i -> 'd0 -> True) 
                            and 'i : (static member op_Explicit: 'i -> int)                             
                            and 'j : (static member (+<): 'j -> 'd1 -> True) 
                            and 'j : (static member op_Explicit: 'j -> int)>() : 't 
                            
                            
                            = x.Item(getN<'i>(), getN<'j>())     
                            

