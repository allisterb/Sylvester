namespace Sylvester.Collections
 
open System

open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10

[<AbstractClass>]
type VArray2D<'dim0, 'dim1 when 'dim0 :> Number and 'dim1 :> Number>() = 
    
    member x.Dim0 = number<'dim0>

    member x.Dim1 = number<'dim1>

    static member inline (!+) (v:VArray2D<'dim0, 'dim1>) = (v.Dim0, v.Dim1)

[<StructuredFormatDisplay("{_Array}")>]
type VArray2D<'dim0, 'dim1, 't when 'dim0 :> Number and 'dim1 :> Number>(items:'t[,]) = 
    inherit VArray2D<'dim0, 'dim1>()
    let l0 = items.GetLength(0)
    let l1 = items.GetLength(1)

    member x._Array = 
        if x.Dim0.IntVal = l0 && x.Dim1.IntVal = l1 then 
            items 
        else 
            raise (new ArgumentOutOfRangeException(sprintf "The dimensions of the array %i %i do not match the dimensions of the type." l0 l1))
    
    member inline x.SetVal(i:'i, j:'j, item:'t) =
        checkidx(i, x.Dim0)
        checkidx(j, x.Dim1)
        x._Array.[i |> int, j |> int] <- item

    member inline x.For(start0:'start0, finish0:'finish0, start1:'start1, finish1:'finish1, f: int -> int -> 't -> unit) =
        checkidx(start0, x.Dim0)
        checkidx(finish0, x.Dim0)
        checkidx(start1, x.Dim1)
        checkidx(finish1, x.Dim1)
        checklt(start0, finish0)
        checklt(start1, finish1)
        for i in ((int) start0)..((int)finish0) do
            for j in ((int) start1)..((int)finish1) do
                f i j x._Array.[i, j]
      
    member inline x.ForAll(f: int -> int -> 't -> unit) =
        for i in 0..(x.Dim0.IntVal - 1) do 
            for j in 0..(x.Dim1.IntVal - 1) do
                f i j x._Array.[i, j]

    member inline x.SetVals(items: 't[,] ) = 
        do 
            if not(x.Dim0.IntVal = items.GetLength(0) && x.Dim1.IntVal = items.GetLength(1)) then 
                raise(ArgumentOutOfRangeException("items"))
            else
                x.ForAll(fun i j a -> x._Array.SetValue(a, i, j))

    member inline x.Item(i:'i, j: 'j) : 't = 
        checkidx(i, x.Dim0)
        checkidx(j, x.Dim1)
        x._Array.[i |> int, j |> int]
           
    member inline x.GetSlice(start0: 'a option, finish0 : 'b option, start1: 'c option, finish1 : 'd option) : VArray2D<'x, 'y, 't> =  
        let inline create(z0:'z0, z1:'z1, items: 't[,] when 'z0 :> Number and 'z1 :> Number) = VArray2D<'z0, 'z1, 't>(items)

        checkidx(start0.Value, x.Dim0)
        checkidx(finish0.Value, x.Dim0)
        checkidx(start1.Value, x.Dim1)
        checkidx(finish1.Value, x.Dim1)
        checklt(start0.Value, finish0.Value)
        checklt(start1.Value, finish1.Value)
        let _start0, _finish0 = start0.Value, finish0.Value
        let _start1, _finish1 = start1.Value, finish1.Value
        let intstart0, intfinish0 = _start0 |> int, _finish0 |> int
        let intstart1, intfinish1 = _start1 |> int, _finish1 |> int
        let length0 = (_finish0 - _start0) + one
        let length1 = (_finish1 - _start1) + one

        create(length0, length1, x._Array.[intstart0..intfinish0, intstart1..intfinish1])
        
    new(x:'t) = VArray2D<'dim0, 'dim1, 't>(Array2D.create (number<'dim0>.IntVal) (number<'dim1>.IntVal) (x))
    
    static member inline VArray = _true

      