namespace Sylvester.Collections

open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10

type HList = interface end

type HCons<'a, 'b when 'b :> HList>  = HCons of 'a * 'b with
    interface HList
    static member inline (|*|) (f, HCons(x, xs)) = f $ HCons(x, xs) 
    static member inline (!?) (HCons(x, xs)) = _false
    static member inline (^+^) (HCons(x, xs), y) = HCons(HCons(x, xs), y)
    static member inline (^+^) (y, HCons(x, xs)) = HCons(y, HCons(x, xs))
    static member inline (^*^) (x, y) = (HAppend $ x) <| y
    static member inline (^<|^) (mapper:HMapper<'a>, x) = mapper $ x
    static member inline (^<|-^) (folder:HFolder<'a, 'v>, x) = folder $ x
    static member inline (!+)(HCons(x, xs)) = (!+ xs) + one
      
    static member inline (|@|) (HCons(x, _), _:N1<``0``>) = x
    static member inline (|@|) (HCons(_, HCons(y, _)), _:N1<``1``>) = y
    static member inline (|@|) (HCons(_, HCons(_, HCons(y, _))), _:N1<``2``>) = y
    static member inline (|@|) (HCons(_, HCons(_, HCons(_, HCons(y, _)))), _:N1<``3``>) = y
    static member inline (|@|) (HCons(_, HCons(_, HCons(_, HCons(_, HCons(y, _))))), _:N1<``4``>) = y
    static member inline (|@|) (HCons(_, HCons(_, HCons(_, HCons(_, HCons(_, HCons(y, _)))))), _:N1<``5``>) = y
    static member inline (|@|) (HCons(_, HCons(_, HCons(_, HCons(_, HCons(_, HCons(_, HCons(y, _))))))), _:N1<``6``>) = y
    static member inline (|@|) (HCons(_, HCons(_, HCons(_, HCons(_, HCons(_, HCons(_, HCons(_, HCons(y, _)))))))), _:N1<``7``>) = y
    static member inline (|@|) (HCons(_, HCons(_, HCons(_, HCons(_, HCons(_, HCons(_, HCons(_, HCons(_, HCons(y, _))))))))), _:N1<``8``>) = y
    static member inline (|@|) (HCons(_, HCons(_, HCons(_, HCons(_, HCons(_, HCons(_, HCons(_, HCons(_, HCons(_, HCons(y, _)))))))))), _:N1<``9``>) = y
     
and HNil = HNil with
    interface HList
    static member inline (|*|) (f, HNil) = f $ HNil
    static member inline (!?)(HNil) = _true
    static member inline (!+)(HNil) = zero
    static member inline (^+^) (x, HNil) = HCons(x, HNil)
   
and HAppend = HAppend with
    static member ($) (HAppend, HNil) = id
    static member inline ($) (HAppend, HCons(x, xs)) = fun list ->
        HCons (x, (HAppend |*| xs) list)

and HMapper<'a> = HMapper of 'a with
    static member ($) (HMapper(_), HNil) = HNil
    static member inline ($) (HMapper(M), HCons(x, xs)) = HCons(M $ x, (HMapper(M) |*| (xs)))

and HFolder<'a, 'v> = HFolder of 'a * 'v with
    static member ($) (HFolder(_, v), HNil) = v
    static member inline ($) (HFolder(F, v), HCons(x, xs)) = HFolder(F, F $ (v,x)) |*| xs