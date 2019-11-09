namespace Sylvester.Tensors

open System

open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10
open Sylvester.Arithmetic.Collections

[<StructuredFormatDisplay("{Val}")>]
type Scalar<'t when 't : struct and 't: (new: unit -> 't) and 't:> ValueType and 't :> IEquatable<'t> and 't :> IFormattable>(x:'t) = 
    inherit Tensor<'t, _0, _0, _0, _0, _0, _0, _0, _0, _0, _0>()

    member val Val = x
    
    static member inline (+) (l:Scalar<single>, r:Scalar<single>) = Scalar<single>(l.Val + r.Val)
    static member inline (+) (l:Scalar<float>, r:Scalar<float>) = Scalar<float>(l.Val + r.Val)
    static member inline (+) (l:Scalar<int32>, r:Scalar<int32>) = Scalar<int32>(l.Val + r.Val)
    static member inline (+) (l:Scalar<uint32>, r:Scalar<uint32>) = Scalar<uint32>(l.Val + r.Val)
    static member inline (+) (l:Scalar<int16>, r:Scalar<int16>) = Scalar<int16>(l.Val + r.Val)
    static member inline (+) (l:Scalar<uint16>, r:Scalar<uint16>) = Scalar<uint16>(l.Val + r.Val)
    static member inline (+) (l:Scalar<int64>, r:Scalar<int64>) = Scalar<int64>(l.Val + r.Val)
    static member inline (+) (l:Scalar<uint64>, r:Scalar<uint64>) = Scalar<uint64>(l.Val + r.Val)
    static member inline (+) (l:Scalar<decimal>, r:Scalar<decimal>) = Scalar<decimal>(l.Val + r.Val)

    static member inline (*) (l:Scalar<single>, r:Scalar<single>) = Scalar<single>(l.Val * r.Val)
    static member inline (*) (l:Scalar<float>, r:Scalar<float>) = Scalar<float>(l.Val * r.Val)
    static member inline (*) (l:Scalar<int32>, r:Scalar<int32>) = Scalar<int32>(l.Val * r.Val)
    static member inline (*) (l:Scalar<uint32>, r:Scalar<uint32>) = Scalar<uint32>(l.Val * r.Val)
    static member inline (*) (l:Scalar<int16>, r:Scalar<int16>) = Scalar<int16>(l.Val * r.Val)
    static member inline (*) (l:Scalar<uint16>, r:Scalar<uint16>) = Scalar<uint16>(l.Val * r.Val)
    static member inline (*) (l:Scalar<int64>, r:Scalar<int64>) = Scalar<int64>(l.Val * r.Val)
    static member inline (*) (l:Scalar<uint64>, r:Scalar<uint64>) = Scalar<uint64>(l.Val * r.Val)
    static member inline (*) (l:Scalar<decimal>, r:Scalar<decimal>) = Scalar<decimal>(l.Val * r.Val)

    static member inline (-) (l:Scalar<single>, r:Scalar<single>) = Scalar<single>(l.Val - r.Val)
    static member inline (-) (l:Scalar<float>, r:Scalar<float>) = Scalar<float>(l.Val - r.Val)
    static member inline (-) (l:Scalar<int32>, r:Scalar<int32>) = Scalar<int32>(l.Val - r.Val)
    static member inline (-) (l:Scalar<uint32>, r:Scalar<uint32>) = Scalar<uint32>(l.Val - r.Val)
    static member inline (-) (l:Scalar<int16>, r:Scalar<int16>) = Scalar<int16>(l.Val - r.Val)
    static member inline (-) (l:Scalar<uint16>, r:Scalar<uint16>) = Scalar<uint16>(l.Val - r.Val)
    static member inline (-) (l:Scalar<int64>, r:Scalar<int64>) = Scalar<int64>(l.Val - r.Val)
    static member inline (-) (l:Scalar<uint64>, r:Scalar<uint64>) = Scalar<uint64>(l.Val - r.Val)
    static member inline (-) (l:Scalar<decimal>, r:Scalar<decimal>) = Scalar<decimal>(l.Val - r.Val)

    static member inline (/) (l:Scalar<single>, r:Scalar<single>) = Scalar<single>(l.Val / r.Val)
    static member inline (/) (l:Scalar<float>, r:Scalar<float>) = Scalar<float>(l.Val / r.Val)
    static member inline (/) (l:Scalar<int32>, r:Scalar<int32>) = Scalar<int32>(l.Val / r.Val)
    static member inline (/) (l:Scalar<uint32>, r:Scalar<uint32>) = Scalar<uint32>(l.Val / r.Val)
    static member inline (/) (l:Scalar<int16>, r:Scalar<int16>) = Scalar<int16>(l.Val / r.Val)
    static member inline (/) (l:Scalar<uint16>, r:Scalar<uint16>) = Scalar<uint16>(l.Val / r.Val)
    static member inline (/) (l:Scalar<int64>, r:Scalar<int64>) = Scalar<int64>(l.Val / r.Val)
    static member inline (/) (l:Scalar<uint64>, r:Scalar<uint64>) = Scalar<uint64>(l.Val / r.Val)
    static member inline (/) (l:Scalar<decimal>, r:Scalar<decimal>) = Scalar<decimal>(l.Val / r.Val)
