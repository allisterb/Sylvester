namespace Sylvester.Tensors

open System

open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10

type IVector<'n, 't when 'n :> Number and 't : struct and 't: (new: unit -> 't) and 't:> ValueType and 't :> IEquatable<'t> and 't :> IFormattable> = 
    inherit IPartialShape<one>
    
    abstract member Dim0:'n

    
    