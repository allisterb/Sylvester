namespace Sylvester.Collections

[<AutoOpen>]
module Logic =

    open System
    open System.Collections.Generic
    open Sylvester.Arithmetic
    open Sylvester.Arithmetic.N10

    let inline varray (n: 'n when 'n :> Number) (arr:'t[]) = VArray<'n, 't>(arr)
    
    let inline vainit (items:IEnumerable<'t>) (vl:VArray<'n, 't>) = vl.SetVals items

    let inline vanew (n: 'n when 'n :> Number) =  VArray<'n, 't>(n, Unchecked.defaultof<'t>) 

    let inline vacopy (n:'n when 'n:>Number) (items:IEnumerable<'t>) =  vanew n |> vainit items

    let inline va2darray (dim0:'dim0) (dim1:'dim1) (x:'t[,]) = VArray2D<'dim0, 'dim1, 't> (x)

    let inline va2dnew (dim0:'dim0 when 'dim0 :> Number) (dim1:'dim1 when 'dim1 :> Number) (x:'t) =
        VArray2D<'dim0, 'dim1, 't>(Unchecked.defaultof<'t>)

    let inline va2dcopy (items: 't[,]) (vl:VArray2D<'dim0, 'dim1, 't>) = vl.SetVals items

