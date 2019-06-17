namespace Sylvester.Arithmetic.Collections
 
open System.Collections.Generic
open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10
open System

type VArray<'n, 't when 'n: (static member Zero : N0) and 'n : (static member op_Explicit: 'n -> int)> () = 

    static member inline VArray = _true

    static member inline (!+) = getN<'n>()

    static member inline (^+^) (x:VArray<'n, 't>, y: VArray<'m, 't>) = x ^+^ y ^+^ VNil
    member inline x.Length = getN<'n>()

    member inline x.IntLength = x.Length |> int
   
    member inline x._Array = Array.create x.IntLength Unchecked.defaultof<'t>
        
  
    member inline x.Item(i:'i when 'i : (static member (+<): 'i -> 'n -> True)) : 't = x._Array.[i |> int]
        
    member inline x.GetSlice(start : 'a option, 
                                        finish : 'b option when ('a or 'b): (static member Zero : N0) 
                                                            and 'a : (static member op_Explicit: 'a -> int) and 'b : (static member op_Explicit: 'b -> int)
                                                            and 'a : (static member (+<): 'a -> 'n -> True) 
                                                            and 'b : (static member (+<): 'b -> 'n -> True)) : VArray<'c, 't> = 
                                       
                                                            let inline create(n:'x when 'x: (static member Zero : N0) and 'x : (static member op_Explicit: 'x -> int)) = VArray<'x,'t>()
                                                            let _start, _finish = start.Value, finish.Value 
                                                            
                                                            let intstart = _start |> int
                                                            let intfinish = _finish |> int
                                                            let count = _finish - _start
                                                            let intcount = count |> int
                                                                
                                                            let v = create(count)
                                                            //let slice = x._List.GetRange(intstart, intfinish - 1)
                                                            v
                                                            //TODO for i in 0 .. intcount - 1 do v.SetVal(i, )
                                                       
    member inline x.SetVal(i:'i, item:'t when 'i : (static member (+<): 'i -> 'n -> True)) = 
        x._Array.[i |> int] <- item

    member inline x.SetVal(i:int, item:'t) =
        if i < (x.IntLength) then  x._Array.[i |> int] <- item else failwith "Index out of range."

    member inline x.SetVals(y: VArray<'n, 't>) = for i in [0..x.IntLength] do x.SetVal(i, y._Array.[i])

    member inline x.At<'i when  'i : (static member Zero : N0) 
                            and 'i : (static member (+<): 'i -> 'n -> True) 
                            and 'i : (static member op_Explicit: 'i -> int)>() : 't = x.Item(getN<'i>())               