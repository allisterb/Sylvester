namespace Sylvester.Arithmetic.Collections

open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10

type VLists = interface end

type VCons<'a, 'b when 'b :> VLists>  = VCons of 'a * 'b with
    interface VLists
    static member inline (|*|) (f, VCons(x, xs)) = f $ VCons(x, xs) 
    static member inline (!?) (VCons(x, xs)) = _false
    static member inline (^+^) (x: 'x when 'x: (static member inline VList: True), VCons(y, ys)) = VCons(x, VCons(y, ys))
    static member inline (^+^) (VCons(x, xs), y:'y when 'y: (static member inline VList: True)) = VCons(VCons(x, xs), y)
    static member inline (^+^) (y:VCons<'d, 'e>, VCons(x, xs)) = VCons(y, VCons(x, xs))
    //static member inline (+) (y:VCons<'d, 'e>, VCons(x, xs)) = VCons(y, VCons(x, xs))
    static member inline (^+++*^) (x, y) = (VAppend $ x) <| y
    static member inline (^<|^) (mapper:VMapper<'a>, x) = mapper $ x
    static member inline (^<|-^) (folder:VFolder<'a, 'v>, x) = folder $ x
    static member inline (!+)(VCons(x, xs)) = (!+ xs) + one 
      
    static member inline (|@|) (VCons(x, _), _:N1<_0>) = x
    static member inline (|@|) (VCons(_, VCons(y, _)), _:N1<_1>) = y
    static member inline (|@|) (VCons(_, VCons(_, VCons(y, _))), _:N1<_2>) = y
    static member inline (|@|) (VCons(_, VCons(_, VCons(_, VCons(y, _)))), _:N1<_3>) = y
    static member inline (|@|) (VCons(_, VCons(_, VCons(_, VCons(_, VCons(y, _))))), _:N1<_4>) = y
    static member inline (|@|) (VCons(_, VCons(_, VCons(_, VCons(_, VCons(_, VCons(y, _)))))), _:N1<_5>) = y
    static member inline (|@|) (VCons(_, VCons(_, VCons(_, VCons(_, VCons(_, VCons(_, VCons(y, _))))))), _:N1<_6>) = y
    static member inline (|@|) (VCons(_, VCons(_, VCons(_, VCons(_, VCons(_, VCons(_, VCons(_, VCons(y, _)))))))), _:N1<_7>) = y
    static member inline (|@|) (VCons(_, VCons(_, VCons(_, VCons(_, VCons(_, VCons(_, VCons(_, VCons(_, VCons(y, _))))))))), _:N1<_8>) = y
    static member inline (|@|) (VCons(_, VCons(_, VCons(_, VCons(_, VCons(_, VCons(_, VCons(_, VCons(_, VCons(_, VCons(y, _)))))))))), _:N1<_9>) = y
     
and VNil = VNil with
    interface VLists
    static member inline (|*|) (f, VNil) = f $ VNil
    static member inline (!?)(VNil) = _true
    static member inline (!+)(VNil) = zero
    static member inline (^+^) (x:'v when 'v: (static member inline VList: True) , VNil) = VCons(x, VNil)
    static member inline (+) (x:'v when 'v: (static member inline VList: True) , VNil) = VCons(x, VNil)
    
and VAppend = VAppend with
    static member ($) (VAppend, VNil) = id
    static member inline ($) (VAppend, VCons(x, xs)) = fun list ->
        VCons (x, (VAppend |*| xs) list)

// Mapper Construct for VList
and VMapper<'a> = VMapper of 'a with
    static member ($) (VMapper(M), VNil) = VNil
    static member inline ($) (VMapper(M), VCons(x, xs)) = VCons(M $ x, (VMapper(M) |*| (xs)))

// Folder Construct for VList
and VFolder<'a, 'v> = VFolder of 'a * 'v with
    static member ($) (VFolder(F, v), VNil) = v
    static member inline ($) (VFolder(F, v), VCons(x, xs)) = VFolder(F, F $ (v,x)) |*| xs

and VLength = VLength with
    static member ($) (VLength, VNil) = zero
    static member inline ($) (VLength, VCons(x, xs)) = (VLength |*| xs) + one

and VLists<'n when 'n: (static member Zero : N0) and 'n : (static member op_Explicit: 'n -> int)> = interface end

and VLists<'n, 'V when 'n: (static member Zero : N0) and 'n : (static member op_Explicit: 'n -> int) 
                    and 'V : (static member inline (!+) : 'V -> 'n)>  = VLists of 'n * 'V with 
        interface VLists<'n>
        
        static member inline Zero = VLists(zero, VNil)
        
        static member inline Length(VLists(x, xs)) = (x, xs) |> fst

        static member inline Unwrap(VLists(x, xs)) = (x, xs) |> snd
        
        static member inline (!+)(VLists(x, xs)) = !+ xs

        static member inline (+) (VLists(c, x), VLists(d, y) ) = VLists(c + d, VCons(x, y))

        static member inline (|@|) (VLists(_, l), n) = (l |@| n) 