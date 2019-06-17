namespace Sylvester.Arithmetic.Collections

[<AutoOpen>]
module Logic =

    open System
    open System.Collections.Generic
    open Sylvester.Arithmetic
    open Sylvester.Arithmetic.N10

    type VDim<'n when 'n: (static member Zero : N0) and 'n : (static member op_Explicit: 'n -> int)> = interface end

   
    let inline vlinit (items:IEnumerable<'t>) (vl:VList<'n, 't>) =
            do if Seq.length items <> vl.IntLength then raise(ArgumentOutOfRangeException("items"))
            vl

    let inline vlist<'n, 't when 'n: (static member Zero : N0) 
                                and 'n : (static member op_Explicit: 'n -> int)> (n:'n) (items: IEnumerable<'t>) = 
                                VList<'n, 't>() |> vlinit (items)

    let inline hlistn(list) = HList(!+ list, list)

    let inline hlist x = x ^+^ HNil |> hlistn 
    
    let inline hlist2 x y = x ^+^ y ^+^ HNil |> hlistn
    
    let inline hlist3 x y z = x ^+^ y ^+^ z ^+^ HNil |> hlistn

    let inline hlist4 x y z a = x ^+^ y ^+^ z ^+^ a ^+^ HNil |> hlistn

    let inline hlist5 x y z a b = x ^+^ y ^+^ z ^+^ a ^+^ b ^+^ HNil |> hlistn

    let inline hlist6 x y z a b c = x ^+^ y ^+^ z ^+^ a ^+^ b ^+^ c ^+^ HNil |> hlistn

    let inline hlist7 x y z a b c d = x ^+^ y ^+^ z ^+^ a ^+^ b ^+^ c ^+^ d ^+^ HNil |> hlistn



    let z = vlist one [1] ^+^ vlist one [1] ^+^ VNil 
