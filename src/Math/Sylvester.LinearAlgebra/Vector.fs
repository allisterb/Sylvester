namespace Sylvester

open System

open Sylvester.Arithmetic
open Sylvester.Collections

[<StructuredFormatDisplay("{Display}")>]
type Vector<'dim0, 't when 'dim0 :> Number and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable>
    ([<ParamArray>] data: 't array) =
    member val Dim0:'dim0 = number<'dim0>
    member val Array = Array<'dim0, 't>(data)
    member x.Display = sprintf "Vector<%i>" (x.Dim0.IntVal)     
    static member ofArray(data: 't array) = Vector(data)

type Vec<'dim0, 't when 'dim0 :> Number and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable> =
    Vector<'dim0, 't>

type Vec<'dim0 when 'dim0 :> Number> = Vec<'dim0, float>

