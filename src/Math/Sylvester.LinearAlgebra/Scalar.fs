namespace Sylvester.LinearAlgebra

open System

open Sylvester.Arithmetic

[<StructuredFormatDisplay("<Scalar>")>]
type Scalar<'t when 't: struct and 't:> ValueType and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable>
    (ops:ILinearAlgebraOps, value:'t) = class end
  
        
 