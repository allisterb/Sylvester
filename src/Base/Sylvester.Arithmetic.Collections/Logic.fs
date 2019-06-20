namespace Sylvester.Arithmetic.Collections

[<AutoOpen>]
module Logic =

    open System
    open System.Collections.Generic
    open Sylvester.Arithmetic
    open Sylvester.Arithmetic.N10

    (*
    type VDim<'n when 'n: (static member Zero : N0) and 'n : (static member op_Explicit: 'n -> int)> = interface end

    let inline vainit (items:IEnumerable<'t>) (vl:VArray<'n, 't>) =
            do if Seq.length items <> vl.IntLength then raise(ArgumentOutOfRangeException("items"))
            Seq.iteri (fun i x -> vl.SetVal(i, x)) items
            vl

    let inline varray<'n, 't when 'n: (static member Zero : N0) 
                             and 'n : (static member op_Explicit: 'n -> int)> (items: IEnumerable<'t>) = 
        VArray<'n, 't>() |> vainit (items)

    let inline vanew<'n, 't when 'n: (static member Zero : N0) 
                            and 'n : (static member op_Explicit: 'n -> int)>  (x:'t) = 
        let intlength = getN<'n>() |> int in varray<'n , 't> [for i in 0..intlength - 1 do yield x]

    let inline vanew'<'n, 't when 'n: (static member Zero : N0) 
                            and 'n : (static member op_Explicit: 'n -> int)> = vanew<'n, 't> Unchecked.defaultof<'t>
                            
    let inline va2dinit (items:'t[,]) (vl:VArray2D<'d0, 'd1, 't>) =
            for i in 0..vl.IntLength0 - 1 do 
                for j in 0 ..vl.IntLength1 - 1 do
                    vl.SetVal(i, j, items.[i, j])
            vl

    let inline varray2d<'d0, 'd1, 't when 'd0: (static member Zero : N0) and 'd1: (static member Zero : N0) 
                                     and 'd0 : (static member op_Explicit: 'd0 -> int)
                                     and 'd1 : (static member op_Explicit: 'd1 -> int)> (items: 't[,]) = 
        VArray2D<'d0, 'd1, 't>() |> va2dinit (items)

    let inline va2dnew<'d0, 'd1, 't when 'd0: (static member Zero : N0) and 'd1: (static member Zero : N0) 
                                     and 'd0 : (static member op_Explicit: 'd0 -> int)
                                     and 'd1 : (static member op_Explicit: 'd1 -> int)> (x:'t) 
                                     = 
        let intlength0 = getN<'d0>() |> int 
        let intlength1 = getN<'d1>() |> int
        let v = VArray2D<'d0, 'd1, 't>()
        for i in 0..intlength0 - 1 do
            for j in 0..intlength1 - 1 do
                v.SetVal(i, j, x)
        v
    
    let inline va2dnew'<'d0, 'd1, 't when 'd0: (static member Zero : N0) and 'd1: (static member Zero : N0) 
                                     and 'd0 : (static member op_Explicit: 'd0 -> int)
                                     and 'd1 : (static member op_Explicit: 'd1 -> int)> = 
        va2dnew<'d0, 'd1, 't> Unchecked.defaultof<'t>
    *)
    let inline varrays (list) = VArrays(!+list, list) 

    let inline hlistn(list) = HList(!+ list, list)

    let inline hlist x = x ^+^ HNil |> hlistn 
    
    let inline hlist2 x y = x ^+^ y ^+^ HNil |> hlistn
    
    let inline hlist3 x y z = x ^+^ y ^+^ z ^+^ HNil |> hlistn

    let inline hlist4 x y z a = x ^+^ y ^+^ z ^+^ a ^+^ HNil |> hlistn

    let inline hlist5 x y z a b = x ^+^ y ^+^ z ^+^ a ^+^ b ^+^ HNil |> hlistn

    let inline hlist6 x y z a b c = x ^+^ y ^+^ z ^+^ a ^+^ b ^+^ c ^+^ HNil |> hlistn

    let inline hlist7 x y z a b c d = x ^+^ y ^+^ z ^+^ a ^+^ b ^+^ c ^+^ d ^+^ HNil |> hlistn
