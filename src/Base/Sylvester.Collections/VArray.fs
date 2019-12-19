namespace Sylvester.Collections
 
open System
open System.Collections.Generic
open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10

type VArray<'d10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1 when 'd10 :> Base10Digit and 'd9 :> Base10Digit 
and 'd8 :> Base10Digit and 'd7 :> Base10Digit and 'd6 :> Base10Digit
and 'd5 :> Base10Digit and 'd4 :> Base10Digit and 'd3 :> Base10Digit and 'd2 :> Base10Digit 
and 'd1 :> Base10Digit>() = class end

[<StructuredFormatDisplay("{_Array}")>]
type VArray<'d10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1, 't when 'd10 :> Base10Digit and 'd9 :> Base10Digit 
                and 'd8 :> Base10Digit and 'd7 :> Base10Digit and 'd6 :> Base10Digit
                and 'd5 :> Base10Digit and 'd4 :> Base10Digit and 'd3 :> Base10Digit and 'd2 :> Base10Digit 
                and 'd1 :> Base10Digit>(n:N10<'d10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>, items:'t[]) = 
    inherit VArray<'d10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>()
    member val _Array = if items.Length = n.IntVal then items else raise(ArgumentOutOfRangeException("items", sprintf "The initializing array length %i does not match %i." items.Length n.IntVal))
    
    member val Length = n

    member val IntLength = n.IntVal
         
    new(n:N10<'d10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>, x:'t) = 
        VArray<'d10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1, 't>(n, 
            Array.create (getN<N10<'d10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>>.IntVal) x)

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
        do if Seq.length items <> x.IntLength then raise(ArgumentOutOfRangeException("items"))
        x.ForAll(fun i a -> x._Array.SetValue(a, i))

    member inline x.Item(i:'i) : 't = 
        checkidx(i, x.Length)
        x._Array.[i |> int]
           
    member inline x.GetSlice(start: 'a option, finish : 'b option) = 
        let inline create(c:'c, items: 't[] when 'c :> N10<'f10, 'f9, 'f8, 'f7, 'f6, 'f5, 'f4, 'f3, 'f2, 'f1>) = 
            VArray<'f10, 'f9, 'f8, 'f7, 'f6, 'f5, 'f4, 'f3, 'f2, 'f1, 't>(c, items)

        checkidx(start.Value, x.Length)
        checkidx(finish.Value, x.Length)
        checklt(start.Value, finish.Value)
        let _start, _finish = start.Value, finish.Value            
        let intstart, intfinish = _start |> int, _finish |> int
        let length = (_finish - _start) + one  

        create(length, x._Array.[intstart..intfinish])
        
    static member inline VArray = _true

    static member inline (!+) (v:VArray<'d10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1, 't>) = v.Length 


 type VArray<'d10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1 when 'd10 :> Base10Digit and 'd9 :> Base10Digit 
 and 'd8 :> Base10Digit and 'd7 :> Base10Digit and 'd6 :> Base10Digit
 and 'd5 :> Base10Digit and 'd4 :> Base10Digit and 'd3 :> Base10Digit and 'd2 :> Base10Digit 
 and 'd1 :> Base10Digit> with
    static member create(arr: 't[]) = new VArray<'d10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1, 't>(new N10<'d10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>(), arr)
 
 type VArray<'d1 when 'd1 :> Base10Digit> = VArray<``0``, ``0``, ``0``, ``0``, ``0``, ``0``, ``0``, ``0``, ``0``, 'd1>

 type VArray<'d2, 'd1 when 'd1 :> Base10Digit and 'd2 :> Base10Digit> = VArray<``0``, ``0``, ``0``, ``0``, ``0``, ``0``, ``0``, ``0``, 'd2, 'd1>

 type VArray<'d3, 'd2, 'd1 when 'd1 :> Base10Digit and 'd2 :> Base10Digit and 'd3 :> Base10Digit> = VArray<``0``, ``0``, ``0``, ``0``, ``0``, ``0``, ``0``, 'd3, 'd2, 'd1>


