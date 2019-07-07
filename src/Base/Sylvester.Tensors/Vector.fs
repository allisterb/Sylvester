namespace Sylvester.Tensors

open System
open MathNet.Numerics.LinearAlgebra

open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10
open Sylvester.Arithmetic.Collections

[<StructuredFormatDisplay("{_Array}")>]
type Vector<'t, 'd10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1 when 'd10 :> Base10Digit and 'd9 :> Base10Digit 
                and 'd8 :> Base10Digit and 'd7 :> Base10Digit and 'd6 :> Base10Digit
                and 'd5 :> Base10Digit and 'd4 :> Base10Digit and 'd3 :> Base10Digit and 'd2 :> Base10Digit 
                and 'd1 :> Base10Digit
                and 't : struct and 't: (new: unit -> 't) and 't:> ValueType and 't :> IEquatable<'t> and 't :> IFormattable>(n:N10<'d10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>, items:'t[]) = 
    inherit Tensor<'t, _0, _0, _0, _0, _0, _0, _0, _0, _0, _2>()
   
    do if n.IntVal <> items.Length then raise (ArgumentException("items", sprintf "The size of the array parameter %d is not the size of the vector dimension %d." items.Length n.IntVal))

    new(items:'t[]) = Vector<'t, 'd10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>(N10<'d10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>(), items)
    
    new () =  Vector<'t, 'd10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>(Array.create (N10<'d10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>().IntVal) Unchecked.defaultof<'t>)
    
    new(x:'t) = Vector<'t, 'd10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>(Array.create (N10<'d10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>().IntVal) x)
   
    member val Array = varray n items
    
    member val _Array = items

    member val _Vector = DenseVector.ofArray items

    member val Dim0 = N10<'d10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>()

    member x.Dims = x.Array ^+^ VNil |> varrays

    member x.Length = x.Dim0

    member inline x.Create(c:'c, items: 't[] when 'c :> N10<'f10, 'f9, 'f8, 'f7, 'f6, 'f5, 'f4, 'f3, 'f2, 'f1>) = 
            Vector<'t, 'f10, 'f9, 'f8, 'f7, 'f6, 'f5, 'f4, 'f3, 'f2, 'f1>(c, items)

    static member inline Vector = _true
 
    static member inline (!+)  (l:Vector<'t, 'd10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>) = l.Dim0

    static member inline (!@)  (l:Vector<'t, 'd10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>) = l._Array

    static member inline (!@@)  (l:Vector<'t, 'd10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>) = l._Vector

    member inline x.SetVal(i:'i, item: 't) = x.Array.SetVal(i, item)
    
    member inline x.Item(i:'i) = x.Array.[i]

    member inline x.GetSlice(start: 'a option, finish : 'b option) = 
        checkidx(start.Value, x.Dim0)
        checkidx(finish.Value, x.Dim0)
        checklt(start.Value, finish.Value)
        
        let _start, _finish = start.Value, finish.Value            
        let intstart, intfinish = _start |> int, _finish |> int
        let length = (_finish - _start) + one  

        x.Create(length, x._Array.[intstart..intfinish])
 
    member inline x.For(start, finish, f: int -> 't -> unit) = x.Array.For(start, finish, f)

    member inline x.ForAll(f: int -> 't -> unit) = x.Array.ForAll(f)

    static member inline (+) (l:Vector<'t, 'd10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>, r:Vector<'t, 'd10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>) =
       let res = DenseVector.zero<'t>(l.Length.IntVal)
       l._Vector.Add(r._Vector, res)
       Vector<'t, 'd10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>(res.AsArray())

    static member inline (-) (l:Vector<'t, 'd10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>, r:Vector<'t, 'd10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>) =
       let res = DenseVector.zero<'t>(l.Length.IntVal)
       l._Vector.Subtract(r._Vector, res)
       Vector<'t, 'd10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>(res.AsArray())

    static member inline (*) (l:Vector<'t, 'd10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>, r:Vector<'t, 'd10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>) =
       let res = Vector<'t, 'd10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>()
       l._Vector.DotProduct(r._Vector) |> Scalar 

    static member inline (*) (l:Scalar<'t>, r:Vector<'t, 'd10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>) =
        r._Vector.Multiply(l.Val).ToArray() |> Vector<'t, 'd10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>
    
    static member inline (*) (l:Vector<'t, 'd10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>, r: Scalar<'t>) =
        l._Vector.Multiply(r.Val).ToArray() |> Vector<'t, 'd10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>

    static member inline (/) (l:Scalar<'t>, r:Vector<'t, 'd10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>) =
        r._Vector.Divide(l.Val).ToArray() |> Vector<'t, 'd10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>
    
    static member inline (/) (l:Vector<'t, 'd10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>, r: Scalar<'t>) =
        l._Vector.Divide(r.Val).ToArray() |> Vector<'t, 'd10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>

    static member inline (.*) (l:Vector<'t, 'd10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>, r:Vector<'t, 'd10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>) =
       l._Vector.PointwiseMultiply(r._Vector).ToArray() |> Vector<'t, 'd10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1> 

    static member inline (./) (l:Vector<'t, 'd10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>, r:Vector<'t, 'd10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>) =
       let res = Vector<'t, 'd10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>()
       l._Vector.PointwiseDivide(r._Vector).ToArray() |> Vector<'t, 'd10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1> 

    static member inline Zero = Vector<'t, 'd10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>()
    
    static member inline One =  
        let n = N10<'d10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>()
        Vector<'t, 'd10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>(Vector<'t>.Build.One)

    static member inline Rand =  
        let n = N10<'d10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>()
        Vector<'t, 'd10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>(Vector<'t>.Build.Random(n.IntVal).ToArray()) 
       
 
