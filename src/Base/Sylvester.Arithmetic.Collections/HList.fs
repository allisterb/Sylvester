namespace Sylvester.Arithmetic

module HList = 

    open Sylvester.Arithmetic
    open Sylvester.Arithmetic.N10

    type HList = interface end

    type HNil = HNil with
        interface HList
        static member inline (|*|) (f, HNil) = f $ HNil
        static member inline (!?)(HNil) = _true
        static member inline (!+)(HNil) = zero
       
    type HCons<'a, 'b when 'b :> HList>  = HCons of 'a * 'b with
        interface HList
        static member inline (|*|) (f, HCons(x, xs)) = f $ HCons(x, xs) 
        static member inline (!?) (HCons(x, xs)) = _false
        static member inline (!+)(HNil) = zero
        static member inline (!+)(HCons(x, xs)) = (!+ xs) + one

        static member inline (|@|) (HCons(x, _), _:N1<_0>) = x
        static member inline (|@|) (HCons(_, HCons(y, _)), _:N1<_1>) = y
        static member inline (|@|) (HCons(_, HCons(_, HCons(y, _))), _:N1<_2>) = y
        static member inline (|@|) (HCons(_, HCons(_, HCons(_, HCons(y, _)))), _:N1<_3>) = y
        static member inline (|@|) (HCons(_, HCons(_, HCons(_, HCons(_, HCons(y, _))))), _:N1<_4>) = y
        static member inline (|@|) (HCons(_, HCons(_, HCons(_, HCons(_, HCons(_, HCons(y, _)))))), _:N1<_5>) = y
        static member inline (|@|) (HCons(_, HCons(_, HCons(_, HCons(_, HCons(_, HCons(_, HCons(y, _))))))), _:N1<_6>) = y
        static member inline (|@|) (HCons(_, HCons(_, HCons(_, HCons(_, HCons(_, HCons(_, HCons(_, HCons(y, _)))))))), _:N1<_7>) = y
        static member inline (|@|) (HCons(_, HCons(_, HCons(_, HCons(_, HCons(_, HCons(_, HCons(_, HCons(_, HCons(y, _))))))))), _:N1<_8>) = y
        static member inline (|@|) (HCons(_, HCons(_, HCons(_, HCons(_, HCons(_, HCons(_, HCons(_, HCons(_, HCons(_, HCons(y, _)))))))))), _:N1<_9>) = y
         
    type HAppend = HAppend with
        static member ($) (HAppend, HNil) = id
        static member inline ($) (HAppend, HCons(x, xs)) = fun list ->
            HCons (x, (HAppend |*| xs) list)

    let inline (^+^) head tail = HCons(head, tail)
    let inline (^++^) x y = (HAppend $ x) <| y
        
    


    