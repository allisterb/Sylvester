namespace Sylvester.Tensors

[<AutoOpen>]
module Logic =

    open Sylvester.Arithmetic
    open Sylvester.Arithmetic.N10

    let inline scalar (x:'t) = Scalar<'t>(x)

    let inline vec (n:N10<'d10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>) (arr:'t[]) =  
        Vector<'t, 'd10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>(n, arr)

    let inline mat (dim0:N10<'d10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>) (dim1:N10<'e10,'e9, 'e8, 'e7, 'e6, 'e5, 'e4, 'e3, 'e2, 'e1>) (x:'t[,]) =
        Matrix<'t, 'd10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1, 'e10, 'e9, 'e8, 'e7, 'e6, 'e5, 'e4, 'e3, 'e2, 'e1> (dim0, dim1, x)


    




    
