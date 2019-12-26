namespace Sylvester.Tensors

open System

open Sylvester
open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10
open Sylvester.Collections

type IUnknownShape =
    abstract member Rank:Option<int> with get,set
    abstract member Dims:Option<int[]> with get,set

type IPartialShape<'d10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1 when 'd10 :> Base10Digit and 'd9 :> Base10Digit 
                and 'd8 :> Base10Digit and 'd7 :> Base10Digit and 'd6 :> Base10Digit
                and 'd5 :> Base10Digit and 'd4 :> Base10Digit and 'd3 :> Base10Digit and 'd2 :> Base10Digit 
                and 'd1 :> Base10Digit> = 
    inherit IUnknownShape
    
    abstract member Rank:N10<'d10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>
    

type IFullShape<'d10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1 when 'd10 :> Base10Digit and 'd9 :> Base10Digit 
and 'd8 :> Base10Digit and 'd7 :> Base10Digit and 'd6 :> Base10Digit
and 'd5 :> Base10Digit and 'd4 :> Base10Digit and 'd3 :> Base10Digit and 'd2 :> Base10Digit 
and 'd1 :> Base10Digit> = 
    inherit IPartialShape<'d10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>

    abstract member Dims:HList