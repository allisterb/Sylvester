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

    let inline vlist<'n, 't when 'n: (static member Zero : N0) and 'n : (static member op_Explicit: 'n -> int)> (n:'n) (items: IEnumerable<'t>) = VList<'n, 't>() |> vlinit (items)

    let inline hlist x = x ^+^ HNil 
    
    let inline hlist2 x y = x ^+^ y ^+^ HNil
    
    let inline hlist3 x y z = x ^+^ y ^+^ z ^+^ HNil

    let inline hlist4 x y z a = x ^+^ y ^+^ z ^+^ a ^+^ HNil

    let inline hlist5 x y z a b = x ^+^ y ^+^ z ^+^ a ^+^ b ^+^ HNil

    let inline hlist6 x y z a b c = x ^+^ y ^+^ z ^+^ a ^+^ b ^+^ c ^+^ HNil

    let inline hlist7 x y z a b c d = x ^+^ y ^+^ z ^+^ a ^+^ b ^+^ c ^+^ d ^+^ HNil

    let inline hlistn(list) = HList(!+ list, list)

    let r = (vlist one [1]) ^+^ (vlist one [1]) ^+^ VNil



