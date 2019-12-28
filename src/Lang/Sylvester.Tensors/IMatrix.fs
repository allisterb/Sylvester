namespace Sylvester.Tensors

open System

open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10

type IMatrix<'dim0, 'dim1, 't when 'dim0 :> Number and 'dim1 :> Number and 't : struct and 't: (new: unit -> 't) and 't:> ValueType and 't :> IEquatable<'t> and 't :> IFormattable> = 
    inherit IPartialShape<two>
    
    abstract member Dim0:'dim0
    abstract member Dim1:'dim1