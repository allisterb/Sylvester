namespace Sylvester.Arithmetic.Collections
 
open System
open System.Collections.Generic
open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10

[<StructuredFormatDisplay("{_Array}")>]
type VArray<'t, 'd10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1 when 'd10 :> Base10Digit and 'd9 :> Base10Digit 
                and 'd8 :> Base10Digit and 'd7 :> Base10Digit and 'd6 :> Base10Digit
                and 'd5 :> Base10Digit and 'd4 :> Base10Digit and 'd3 :> Base10Digit and 'd2 :> Base10Digit 
                and 'd1 :> Base10Digit>(n:N10<'d10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>, items:'t[]) = 

    member val _Array = if items.Length = n.IntVal then items else raise(ArgumentOutOfRangeException("items", sprintf "The initializing array length %i does not match %i." items.Length n.IntVal))
    
    member val Length = n

    member val IntLength = n.IntVal
         
    new(n:N10<'d10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>, x:'t) = 
        VArray<'t, 'd10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>(n, 
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
           
    member inline x.GetSlice(start: 'a option, finish : 'b option) = //: VArray<'t, 'e10, 'e9, 'e8, 'e7, 'e6, 'e5, 'e4, 'e3, 'e2, 'e1> = 
        let inline create(c:'c, items: 't[] when 'c :> N10<'f10, 'f9, 'f8, 'f7, 'f6, 'f5, 'f4, 'f3, 'f2, 'f1>) = 
            VArray<'t, 'f10, 'f9, 'f8, 'f7, 'f6, 'f5, 'f4, 'f3, 'f2, 'f1>(c, items)

        checkidx(start.Value, x.Length)
        checkidx(finish.Value, x.Length)
        checklt(start.Value, finish.Value)
        let _start, _finish = start.Value, finish.Value            
        let intstart, intfinish = _start |> int, _finish |> int
        let length = (_finish - _start) + one  

        create(length, x._Array.[intstart..intfinish])
        
    static member inline VArray = _true

    static member inline (!+) (v:VArray<'t, 'd10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>) = v.Length 

module X = 
    let j =  new VArray<int, _0, _0, _0, _0, _0, _0, _0, _0, _0, _3>(three, 1) ^+^ new VArray<int, _0, _0, _0, _0, _0, _0, _0, _0, _0, _2>(two, 1) ^+^ VNil



