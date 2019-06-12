namespace Sylvester.Examples.Arithmetic

module VList =
    open System
    open System.Collections.Generic
    
    open Sylvester.Arithmetic
    open Sylvester.Arithmetic.N10

    type VList<'n, 't when 'n: (static member Zero : N0) and 'n : (static member op_Explicit: 'n -> int)>() = 
        
        member inline x.Length = (int)(Activator.CreateInstance<'n>())
       
        member inline x._List = new List<'t>(x.Length)

        member x.Item(i : int) : 'T = Unchecked.defaultof<_>
        member this.Item(name : string) : 'T = Unchecked.defaultof<_>
        member this.GetSlice(a : int option, b : int option) : 'T = Unchecked.defaultof<_>



    let v = new VList<N<100>, int>()



