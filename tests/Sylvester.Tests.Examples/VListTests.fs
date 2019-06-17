namespace Sylvester.Tests.Arithmetic

module VArray = 

    open System.Collections.Generic
    open Sylvester.Arithmetic
    open Sylvester.Arithmetic.N10
    open Xunit
    
    type VArray<'n, 't when 'n: (static member Zero : N0) and 'n : (static member op_Explicit: 'n -> int)>() = 
        
        member inline x.Length = getN<'n>()
       
        member inline x._List = new List<'t>((int) x.Length)

        member inline x.Item(i:'i when 'i : (static member (+<=): 'i -> 'n -> True)) : 't = x._List.Item(i |> int)
            
        /// member this.GetSlice(a : int option, b : int option) : 'T = Unchecked.defaultof<_>
        member inline x.at<'i when  'i : (static member Zero : N0) 
                                and 'i : (static member (+<=): 'i -> 'n -> True) 
                                and 'i : (static member op_Explicit: 'i -> int)>() : 't = x.Item(getN<'i>()) 
    
  