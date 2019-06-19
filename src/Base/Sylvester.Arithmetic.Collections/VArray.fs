namespace Sylvester.Arithmetic.Collections
 
open System
open System.Collections.Generic
open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10

type VArray<'n, 't when 'n: (static member Zero : N0) and 'n : (static member op_Explicit: 'n -> int)> () = 

    static member inline VArray = _true

    static member inline (!+) = getN<'n>()

    static member inline (^+^) (x:VArray<'n, 't>, y: VArray<'m, 't>) = x ^+^ y ^+^ VNil

    member inline x.Length = getN<'n>()

    member inline x.IntLength = x.Length |> int
   
    member inline x._Array = Array.create x.IntLength Unchecked.defaultof<'t>
        
    member inline x.SetVal(i:'i, item:'t when 'i : (static member (+<): 'i -> 'n -> True)) = 
        x._Array.[i |> int] <- item

    member inline x.SetVal(i:int, item:'t) =
        if i < (x.IntLength) then  x._Array.[i |> int] <- item else raise(IndexOutOfRangeException("item"))//r//failwith "Index out of range."

    member inline x.SetVals(items: IEnumerable<'t> ) = 
        do if Seq.length items <> x.IntLength then raise(ArgumentOutOfRangeException("items"))
        Seq.iteri (fun (i:int) a -> x.SetVal(i, a)) items

    member inline x.Item(i:'i when 'i : (static member (+<): 'i -> 'n -> True)) : 't = x._Array.[i |> int]
        
    member inline x.GetSlice(start: 'a option, 
                                        finish : 'b option when ('a or 'b): (static member Zero : N0) 
                                                            and 'a : (static member op_Explicit: 'a -> int) 
                                                            and 'b : (static member op_Explicit: 'b -> int)
                                                            and 'a : (static member (+<): 'a -> 'n -> True) 
                                                            and 'b : (static member (+<): 'b -> 'n -> True)) : VArray<'c, 't> = 
                                       
        let inline create(n:'z, items:IEnumerable<'t> when 'z: (static member Zero : N0) 
                                and 'z : (static member op_Explicit: 'z -> int)) = 
                let v = VArray<'z,'t>()
                v.SetVals items
                v

        let _start, _finish = start.Value, finish.Value            
        let intstart, intfinish = _start |> int, _finish |> int
        let length = _finish - _start  
        let v = create(length, x._Array.[intstart..intfinish])
        v
    
    member inline x.At<'i when  'i : (static member Zero : N0) 
                            and 'i : (static member (+<): 'i -> 'n -> True) 
                            and 'i : (static member op_Explicit: 'i -> int)>() : 't = x.Item(getN<'i>())     
