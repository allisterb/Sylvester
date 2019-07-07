namespace Sylvester.Arithmetic.Collections

[<AutoOpen>]
module Logic =

    open System
    open System.Collections.Generic
    open Sylvester.Arithmetic
    open Sylvester.Arithmetic.N10

    let inline vainit (items:IEnumerable<'t>) (vl:VArray<'t, 'd10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>) =
            vl.SetVals items

    let inline vanew (n:N10<'d10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>) =  
        VArray<'t, 'd10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>(n, Unchecked.defaultof<'t>) 

    let inline vacopy (n:N10<'d10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>, items:IEnumerable<'t>) =  
        VArray<'t, 'd10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>(n, Unchecked.defaultof<'t>) |> vainit (items)

    let inline varray (n:N10<'d10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>) (arr:'t[]) =  
        VArray<'t, 'd10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>(n, arr)

    let inline varray'<'t, 'd10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1 when 'd10 :> Base10Digit and 'd9 :> Base10Digit 
                and 'd8 :> Base10Digit and 'd7 :> Base10Digit and 'd6 :> Base10Digit
                and 'd5 :> Base10Digit and 'd4 :> Base10Digit and 'd3 :> Base10Digit and 'd2 :> Base10Digit 
                and 'd1 :> Base10Digit>(arr:'t[]) = 
        let n = N10<'d10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>()
        VArray<'t, 'd10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>(n, arr)
    
    let inline va2dnew (dim0:N10<'d10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>) (dim1:N10<'e10,'e9, 'e8, 'e7, 'e6, 'e5, 'e4, 'e3, 'e2, 'e1>) (x:'t) =
        VArray2D<'t, 'd10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1, 'e10, 'e9, 'e8, 'e7, 'e6, 'e5, 'e4, 'e3, 'e2, 'e1> (dim0, dim1, x)

    let inline va2dcopy (items: 't[,]) (vl:VArray2D<'t, 'd10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1, 'e10,'e9, 'e8, 'e7, 'e6, 'e5, 'e4, 'e3, 'e2, 'e1>) =
        vl.SetVals items

    let inline va2darray (dim0:N10<'d10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>) (dim1:N10<'e10,'e9, 'e8, 'e7, 'e6, 'e5, 'e4, 'e3, 'e2, 'e1>) (x:'t[,]) =
        VArray2D<'t, 'd10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1, 'e10, 'e9, 'e8, 'e7, 'e6, 'e5, 'e4, 'e3, 'e2, 'e1> (dim0, dim1, x)
    
    let inline varrays (list) = VArrays(!+list, list) 

    let inline hlistn(list) = HList(!+ list, list)

    let inline hlist x = x ^+^ HNil |> hlistn 
    
    let inline hlist2 x y = x ^+^ y ^+^ HNil |> hlistn
    
    let inline hlist3 x y z = x ^+^ y ^+^ z ^+^ HNil |> hlistn

    let inline hlist4 x y z a = x ^+^ y ^+^ z ^+^ a ^+^ HNil |> hlistn

    let inline hlist5 x y z a b = x ^+^ y ^+^ z ^+^ a ^+^ b ^+^ HNil |> hlistn

    let inline hlist6 x y z a b c = x ^+^ y ^+^ z ^+^ a ^+^ b ^+^ c ^+^ HNil |> hlistn

    let inline hlist7 x y z a b c d = x ^+^ y ^+^ z ^+^ a ^+^ b ^+^ c ^+^ d ^+^ HNil |> hlistn

    let inline hh x n = x |@| n