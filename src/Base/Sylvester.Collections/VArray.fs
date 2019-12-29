namespace Sylvester.Collections
 
open System
open System.Collections.Generic

open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10

[<AbstractClass>]
type VArray<'n when 'n :> Number>() = 
    
    member x.Length = number<'n>
    
    member x.IntLength = x.Length.IntVal

    static member inline (!+) (v:VArray<'n>) = v.Length

[<StructuredFormatDisplay("{_Array}")>]
type VArray<'n, 't when 'n :> Number >(items:'t[]) = 
    inherit VArray<'n>()
    
    member x._Array = if items.Length = x.IntLength then items else raise (ArgumentOutOfRangeException("items", sprintf "The initializing array length %i does not match the type legth %i." items.Length x.IntLength))
         
    member inline x.SetVal(i:'i, item:'t) =
        checkidx(i, x.Length)
        x._Array.[i |> int] <- item

    member inline x.For(start:'start, finish:'finish, f: int -> 't -> unit) =
        checkidx(start, x.Length)
        checkidx(finish, x.Length)
        checklt(start, finish)
        for i in ((int) start)..((int)finish) do f i x._Array.[i]
      
    member inline x.ForAll(f: int -> 't -> unit) =
        for i in 0..x._Array.Length - 1 do f i x._Array.[i]

    member inline x.SetVals(items: IEnumerable<'t> ) = 
        do if Seq.length items <> x.IntLength then raise(ArgumentOutOfRangeException("The length of the sequence argument does not match the type length."))
        x.ForAll(fun i a -> x._Array.SetValue(a, i))

    member inline x.Item(i:'i) : 't = 
        checkidx(i, x.Length)
        x._Array.[i |> int]
           
    member inline x.GetSlice(start: 'a option, finish : 'b option) = 
        let inline create(c:'c, items: 't[] when 'c :> Number) = 
            VArray<'n, 't>(items)

        checkidx(start.Value, x.Length)
        checkidx(finish.Value, x.Length)
        checklt(start.Value, finish.Value)
        let _start, _finish = start.Value, finish.Value            
        let intstart, intfinish = _start |> int, _finish |> int
        let length = (_finish - _start) + one  

        create(length, x._Array.[intstart..intfinish])

    member inline x.Map(m:'t->'t) = 
        let arr = VArray<'n, 't()
    
    new(x:'t) = 
        VArray<'n, 't>(Array.create (number<'n>.IntVal) x)

    new() = 
        VArray<'n, 't>(Array.create number<'n>.IntVal Unchecked.defaultof<'t>)

 type VArray<'n when 'n :> Number> with
    static member create(arr: 't[]) = new VArray<'n, 't>(arr)

