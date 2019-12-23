namespace Sylvester.Tensors

open System

open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10

type IMatrix<'t, 'd10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1, 'e10,'e9, 'e8, 'e7, 'e6, 'e5, 'e4, 'e3, 'e2, 'e1 
    when 'd10 :> Base10Digit and 'd9 :> Base10Digit and 'd8 :> Base10Digit and 'd7 :> Base10Digit and 'd6 :> Base10Digit
    and 'd5 :> Base10Digit and 'd4 :> Base10Digit and 'd3 :> Base10Digit and 'd2 :> Base10Digit 
    and 'd1 :> Base10Digit and 'e10 :> Base10Digit and 'e9 :> Base10Digit and 'e8 :> Base10Digit and 'e7 :> Base10Digit and 'e6 :> Base10Digit
    and 'e5 :> Base10Digit and 'e4 :> Base10Digit and 'e3 :> Base10Digit and 'e2 :> Base10Digit 
    and 'e1 :> Base10Digit and 't : struct and 't: (new: unit -> 't) and 't:> ValueType and 't :> IEquatable<'t> and 't :> IFormattable> = 
    
    abstract member Dim0:N10<'d10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>

    abstract member Dim1:N10<'e10,'e9, 'e8, 'e7, 'e6, 'e5, 'e4, 'e3, 'e2, 'e1>


   
    
        

        
        
        
