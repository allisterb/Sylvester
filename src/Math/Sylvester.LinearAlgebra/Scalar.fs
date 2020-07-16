namespace Sylvester.LinearAlgebra

open System

[<StructuredFormatDisplay("<Scalar>")>]
type Scalar<'t when 't: struct and 't:> ValueType and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IComparable and 't :> IFormattable>
    (value:'t) = class end
  
        
 