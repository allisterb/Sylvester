namespace Sylvester.Arithmetic.Collections

module VList = 
    
    open System.Collections.Generic
    
    open Sylvester.Arithmetic
    open Sylvester.Arithmetic.N10

    type VList<'n, 't when 'n: (static member Zero : N0) and 'n : (static member op_Explicit: 'n -> int)>() = 
                
        member inline x.Length = getN<'n>()
       
        member inline internal x._List = 
            let length = x.Length |> int
            let list = new List<'t>(length)
            for i = 1 to length do
                list.Add(Unchecked.defaultof<'t>)
            list

        member inline x.Item(i:'i when 'i : (static member (+<): 'i -> 'n -> True)) : 't = x._List.[i |> int]
            
        /// member this.GetSlice(a : int option, b : int option) : 'T = Unchecked.default
        member inline x.SetVal(i:'i, item:'t when 'i : (static member (+<): 'i -> 'n -> True)) = 
            x._List.Item((i |> int)) <- item

        member inline x.SetVal(i:int, item:'t) =
            if i < (x.Length |> int) then x._List.Item((i)) <- item else failwith "Index out of range"

        member inline x.at<'i when  'i : (static member Zero : N0) 
                                and 'i : (static member (+<): 'i -> 'n -> True) 
                                and 'i : (static member op_Explicit: 'i -> int)>() : 't = x.Item(getN<'i>())
