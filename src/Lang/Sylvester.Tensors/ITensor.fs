namespace Sylvester.Tensors

open System

open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10

type ITensor3<'dim0, 'dim1, 'dim2, 't when 'dim0 :> Number and 'dim1 :> Number and 'dim2 :> Number 
    and 't : struct and 't: (new: unit -> 't) and 't:> ValueType and 't :> IEquatable<'t> and 't :> IFormattable> = 
    inherit IPartialShape<two>
    
    abstract member Dim0:'dim0
    abstract member Dim1:'dim1
    abstract member Dim2:'dim2

type ITensor4<'dim0, 'dim1, 'dim2, 'dim3, 't when 'dim0 :> Number and 'dim1 :> Number and 'dim2 :> Number and 'dim3 :> Number and 't : struct and 't: (new: unit -> 't) and 't:> ValueType and 't :> IEquatable<'t> and 't :> IFormattable> = 
    inherit IPartialShape<three>
    
    abstract member Dim0:'dim0
    abstract member Dim1:'dim1
    abstract member Dim2:'dim2
    abstract member Dim3:'dim3
