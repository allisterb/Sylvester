namespace Sylvester.Collections

[<AutoOpen>]
module Logic =

    open System
    open System.Collections.Generic
    open Sylvester.Arithmetic
    
    let inline vainit (items:IEnumerable<'t>) (vl:VArray<'n, 't>) = vl.SetVals items
    
    let inline varray (n: 'n when 'n :> Number) (arr:'t[]) = VArray<'n, 't>(arr)
    
    let inline vanew (n: 'n when 'n :> Number) =  VArray<'n, 't>() 

    let inline vacopy (n:'n when 'n:>Number) (items:IEnumerable<'t>) =  vanew n |> vainit items

    let inline va2dinit (items: 't[,]) (vl:VArray2D<'dim0, 'dim1, 't>) = vl.SetVals items

    let inline va2darray (dim0:'dim0) (dim1:'dim1) (x:'t[,]) = VArray2D<'dim0, 'dim1, 't> (x)

    let inline va2dnew (dim0:'dim0 when 'dim0 :> Number) (dim1:'dim1 when 'dim1 :> Number) () =
        VArray2D<'dim0, 'dim1, 't>()

    //let inline va2dcopy (dim0:'dim0 when 'dim0:>Number) (dim1:'dim1 when 'dim1:>Number) (items:'t[,]) =  (va2dnew dim0 dim1) |> va2dinit items

    

