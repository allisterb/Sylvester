namespace Sylvester.Collections
 
open System

open Sylvester.Arithmetic

[<AbstractClass>]
type Array2D<'dim0, 'dim1 when 'dim0 :> Number and 'dim1 :> Number>() = 
    
    member x.Dim0 = number<'dim0>

    member x.Dim1 = number<'dim1>

    static member (!+) (v:Array2D<'dim0, 'dim1>) = (v.Dim0, v.Dim1)

[<StructuredFormatDisplay("{_Array}")>]
type Array2D<'dim0, 'dim1, 't when 'dim0 :> Number and 'dim1 :> Number>(items:'t[,]) = 
    inherit Array2D<'dim0, 'dim1>()
    let l0 = items.GetLength(0)
    let l1 = items.GetLength(1)

    member x._Array = 
        if x.Dim0.IntVal = l0 && x.Dim1.IntVal = l1 then 
            items 
        else 
            raise (new ArgumentOutOfRangeException(sprintf "The dimensions of the initializer array %ix%i do not match the dimensions of the type." l0 l1))
    
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
           
    member inline x.GetSlice(start0: 'a option, finish0 : 'b option, start1: 'c option, finish1 : 'd option) : Array2D<'x, 'y, 't> =  
        let inline create(z0:'z0, z1:'z1, items: 't[,] when 'z0 :> Number and 'z1 :> Number) = Array2D<'z0, 'z1, 't>(items)

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
        let length0 = (_finish0 - _start0)
        let length1 = (_finish1 - _start1)

        create(length0, length1, x._Array.[intstart0..intfinish0, intstart1..intfinish1])
        
    new(x:'t) = Array2D<'dim0, 'dim1, 't>(Array2D.create (number<'dim0>.IntVal) (number<'dim1>.IntVal) (x))
    
    new() = Array2D<'dim0, 'dim1, 't>(Array2D.create (number<'dim0>.IntVal) (number<'dim1>.IntVal) (Unchecked.defaultof<'t>))
     
type Array2D<'i0, 'i1 when 'i0 :> Number and 'i1:> Number> with
   static member create(arr: 't[,]) = new Array2D<'i0, 'i1, 't>(arr)
   static member create(v: 't) = new Array2D<'i0, 'i1, 't>(Array2D.create (number<'i0>.IntVal) (number<'i1>.IntVal) (v))

[<AutoOpen>]
module VArray2D =
    open Sylvester.Arithmetic.N10
    
    let inline va2dinit (items: 't[,]) (vl:Array2D<'dim0, 'dim1, 't>) = vl.SetVals items

    let inline va2darray (dim0:'dim0) (dim1:'dim1) (x:'t[,]) = Array2D<'dim0, 'dim1, 't> (x)

    let inline va2dnew<'dim0, 'dim1, 't when 'dim0 :> Number and 'dim1 :> Number> = Array2D<'dim0, 'dim1, 't>()

    let inline va2dcopy (dim0:'dim0) (dim1:'dim1) (items:'t[,]) =  va2dnew<'dim0, 'dim1, 't> |> va2dinit items 