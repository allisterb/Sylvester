namespace Sylvester.Arithmetic.Collections
 
open System

open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10

[<StructuredFormatDisplay("{_Array}")>]
type VArray2D<'t, 'd10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1, 'e10, 'e9, 'e8, 'e7, 'e6, 'e5, 'e4, 'e3, 'e2, 'e1 
when 'd10 :> Base10Digit and 'd9 :> Base10Digit and 'd8 :> Base10Digit and 'd7 :> Base10Digit and 'd6 :> Base10Digit
                and 'd5 :> Base10Digit and 'd4 :> Base10Digit and 'd3 :> Base10Digit and 'd2 :> Base10Digit 
                and 'd1 :> Base10Digit and 'e10 :> Base10Digit and 'e9 :> Base10Digit and 'e8 :> Base10Digit and 'e7 :> Base10Digit and 'e6 :> Base10Digit
                and 'e5 :> Base10Digit and 'e4 :> Base10Digit and 'e3 :> Base10Digit and 'e2 :> Base10Digit 
                and 'e1 :> Base10Digit>(dim0:N10<'d10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>, 
                                        dim1:N10<'e10, 'e9, 'e8, 'e7, 'e6, 'e5, 'e4, 'e3, 'e2, 'e1>, items:'t[,]) = 

    member val _Array = if items.Length = dim0.IntVal * dim1.IntVal then items else raise(ArgumentOutOfRangeException("items"))
    
    member val Length0 = dim0

    member val Length1 = dim1

    member val IntLength0 = dim0.IntVal

    member val IntLength1 = dim1.IntVal
         
    new(dim0:N10<'d10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>, 
        dim1:N10<'e10, 'e9, 'e8, 'e7, 'e6, 'e5, 'e4, 'e3, 'e2, 'e1>, x:'t) = 
        VArray2D<'t, 'd10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1, 
                 'e10, 'e9, 'e8, 'e7, 'e6, 'e5, 'e4, 'e3, 'e2, 'e1>(dim0, dim1, Array2D.create dim0.IntVal dim1.IntVal x)

    member inline x.SetVal(i:'i, j:'j, item:'t) =
        checkidx(i, x.Length0)
        checkidx(j, x.Length1)
        x._Array.[i |> int, j |> int] <- item

    member inline x.For(start0:'start0, finish0:'finish0, start1:'start1, finish1:'finish1, f: int -> int -> 't -> unit) =
        checkidx(start0, x.Length0)
        checkidx(finish0, x.Length0)
        checkidx(start1, x.Length1)
        checkidx(finish1, x.Length1)
        checklt(start0, finish0)
        checklt(start1, finish1)
        for i in ((int) start0)..((int)finish0) do
            for j in ((int) start1)..((int)finish1) do
                f i j x._Array.[i, j]
      
    member inline x.ForAll(f: int -> int -> 't -> unit) =
        for i in 0..(x.IntLength0 - 1) do 
            for j in 0..(x.IntLength1 - 1) do
                f i j x._Array.[i, j]

    member inline x.SetVals(items: 't[,] ) = 
        do if items.Length <> x.IntLength0 then raise(ArgumentOutOfRangeException("items"))
        x.ForAll(fun i j a -> x._Array.SetValue(a, i, j))

    member inline x.Item(i:'i, j: 'j) : 't = 
        checkidx(i, x.Length0)
        checkidx(j, x.Length1)
        x._Array.[i |> int, j |> int]
           
    member inline x.GetSlice(start0: 'a option, finish0 : 'b option, start1: 'c option, finish1 : 'd option) : 
        VArray2D<'t, 'f10, 'f9, 'f8, 'f7, 'f6, 'f5, 'f4, 'f3, 'f2, 'f1, 'g10, 'g9, 'g8, 'g7, 'g6, 'g5, 'g4, 'g3, 'g2, 'g1> =  
        let inline create(z0:'z0, z1:'z1, items: 't[,] when 'z0 :> N10<'f10, 'f9, 'f8, 'f7, 'f6, 'f5, 'f4, 'f3, 'f2, 'f1> 
                                                    and 'z1 :> N10<'g10, 'g9, 'g8, 'g7, 'g6, 'g5, 'g4, 'g3, 'g2, 'g1>) = 
            VArray2D<'t, 'f10, 'f9, 'f8, 'f7, 'f6, 'f5, 'f4, 'f3, 'f2, 'f1, 'g10, 'g9, 'g8, 'g7, 'g6, 'g5, 'g4, 'g3, 'g2, 'g1>(z0, z1, items)

        checkidx(start0.Value, x.Length0)
        checkidx(finish0.Value, x.Length0)
        checkidx(start1.Value, x.Length1)
        checkidx(finish1.Value, x.Length1)
        checklt(start0.Value, finish0.Value)
        checklt(start1.Value, finish1.Value)
        let _start0, _finish0 = start0.Value, finish0.Value
        let _start1, _finish1 = start1.Value, finish1.Value
        let intstart0, intfinish0 = _start0 |> int, _finish0 |> int
        let intstart1, intfinish1 = _start1 |> int, _finish1 |> int
        let length0 = (_finish0 - _start0) + one
        let length1 = (_finish1 - _start1) + one

        create(length0, length1, x._Array.[intstart0..intfinish0, intstart1..intfinish1])
        
    static member inline VArray = _true

    static member inline (!+) (v:VArray2D<'t, 'd10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1, 'e10, 'e9, 'e8, 'e7, 'e6, 'e5, 'e4, 'e3, 'e2, 'e1>) 
        = (v.Length0, v.Length1) 

    static member inline (^+^) (x:VArray2D<'t, 'd10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1, 'e10, 'e9, 'e8, 'e7, 'e6, 'e5, 'e4, 'e3, 'e2, 'e1>, 
                                y:VArray2D<'t, 'zd10, 'zd9, 'zd8, 'zd7, 'zd6, 'zd5, 'zd4, 'zd3, 'zd2, 'zd1, 'ze10, 'ze9, 'ze8, 'ze7, 'ze6, 'ze5, 'ze4, 'ze3, 'ze2, 'ze1>) 
        = x ^+^ y ^+^ VNil   
    