namespace Sylvester

open System

open Base10

module Arithmetic = 

    type Number = 
      abstract Val:int64
      abstract IntVal:int
      abstract UIntVal:uint64
    
    let number<'n when 'n :> Number> = Activator.CreateInstance<'n>()

    type N10Overflow = N10Overflow
    
    type N10Underflow = N10Underflow

    let inline isZero (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)  = 
            (!?? x10) * (!?? x9) * (!?? x8) * (!?? x7) * (!?? x6) * (!?? x5) * (!?? x4) * (!?? x3) * (!?? x2) * (!?? x1)

    let inline isNotZero (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)  = !! (isZero (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10))

    let inline noDigit1 (_, _, _, _, _, _, _,_, _, x1)  =  (!?? x1) 

    let inline noDigit2 (_, _, _, _, _, _, _, _, x2, _)  = (!?? x2)

    let inline noDigit3 (_, _, _, _, _, _, _, x3,_, _)  = (!?? x3)

    let inline noDigit4 (_, _, _, _, _, _, x4,_, _, _)  = (!?? x4)

    let inline noDigit5 (_, _, _, _, _, x5, _,_, _, _)  = (!?? x5)

    let inline noDigit6 (_, _, _, _, x6, _, _,_, _, _)  = (!?? x6)

    let inline noDigit7 (_, _, _, x7, _, _, _,_, _, _)  = (!?? x7)

    let inline noDigit8 (_, _,x8, _, _, _, _,_, _, _)  = (!?? x8)

    let inline noDigit9 (_, x9, _, _, _, _, _,_, _, _)  = (!?? x9)

    let inline noDigit10 (x10, _, _, _, _, _, _,_, _, _)  = (!?? x10)

    [<StructuredFormatDisplay("nat<{IntVal}>")>]
    type N10<'d10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1 when 'd10 :> Base10Digit and 'd9 :> Base10Digit and 'd8 :> Base10Digit and 'd7 :> Base10Digit and 'd6 :> Base10Digit
        and 'd5 :> Base10Digit and 'd4 :> Base10Digit and 'd3 :> Base10Digit and 'd2 :> Base10Digit and 'd1 :> Base10Digit> 
        (n10: 'd10, n9:'d9, n8:'d8, n7:'d7, n6:'d6, n5:'d5, n4:'d4, n3:'d3, n2:'d2, n1:'d1)  = 

        interface Number with 
            member val UIntVal = n1.Val_ * 1UL + n2.Val_ * 10UL + n3.Val_ * 100UL + n4.Val_ * 1000UL + n5.Val_ * 10000UL + n6.Val_ * 100000UL + n7.Val_ * (1000000UL) + n8.Val_ * (10000000UL) + n9.Val_ * (100000000UL) + n10.Val_ * (1000000000UL) with get
            member val Val = Convert.ToInt64(n1.Val_ * 1UL + n2.Val_ * 10UL + n3.Val_ * 100UL + n4.Val_ * 1000UL + n5.Val_ * 10000UL + n6.Val_ * 100000UL + n7.Val_ * (1000000UL) + n8.Val_ * (10000000UL) + n9.Val_ * (100000000UL) + n10.Val_ * (1000000000UL)) with get            
            member val IntVal = Checked.int(n1.Val * 1 + n2.Val * 10 + n3.Val * 100 + n4.Val * 1000 + n5.Val * 10000 + n6.Val * 100000 + n7.Val * (1000000) + n8.Val * (10000000) + n9.Val * (100000000) + n10.Val * (1000000000)) with get

        member x.Nat = x :> Number

        member x.IntVal = x.Nat.IntVal

        member val UIntVal = n1.Val_ * 1UL + n2.Val_ * 10UL + n3.Val_ * 100UL + n4.Val_ * 1000UL + n5.Val_ * 10000UL + n6.Val_ * 100000UL + n7.Val_ * (1000000UL) + n8.Val_ * (10000000UL) + n9.Val_ * (100000000UL) + n10.Val_ * (1000000000UL)
        
        member val Digits = (n10, n9, n8, n7, n6, n5, n4, n3, n2, n1)

        member val Digit1 = n1

        member val Digit2 = n2

        member val Digit3 = n3

        member val Digit4 = n4

        member val Digit5 = n5

        member val Digit6 = n6

        member val Digit7 = n7

        member val Digit8 = n8

        member val Digit9 = n9

        member val Digit10 = n10

        new() = N10(digit<'d10>(), digit<'d9>(), digit<'d8>(), digit<'d7>(), digit<'d6>(), digit<'d5>(), digit<'d4>(), digit<'d3>(), digit<'d2>(), digit<'d1>())

        static member Zero = N10(d0, d0, d0, d0, d0, d0, d0, d0, d0, d0)

        static member One = N10(d0, d0, d0, d0, d0, d0, d0, d0, d0, d1)

        static member inline (!?) (l: N10<'ld10, 'ld9, 'ld8, 'ld7, 'ld6, 'ld5, 'ld4, 'ld3, 'ld2, 'ld1>) = isZero(l.Digits)

        static member inline (!!?) (l: N10<'ld10, 'ld9, 'ld8, 'ld7, 'ld6, 'ld5, 'ld4, 'ld3, 'ld2, 'ld1>) = !!(!? l)
    
        static member inline (!!!) (a: N10<_, _, _, _, _, _, _, _, _, _>) =
            let (a10, a9, a8, a7, a6, a5, a4, a3, a2, a1) = a.Digits
            N10(!! a10, !! a9, !! a8, !!a7, !!a6, !! a5, !! a4, !! a3, !! a2, !! a1)

        static member inline (!<<<) (a: N10<_, _, _, _, _, _, _, _, _, _>) =
            let (_, a9, a8, a7, a6, a5, a4, a3, a2, a1) = a.Digits
            N10(a9, a8, a7, a6, a5, a4, a3, a2, a1, d0)

        static member inline (!>>>) (a: N10<_, _, _, _, _, _, _, _, _,_>) =
            let (a10, a9, a8, a7, a6, a5, a4, a3, a2, _) = a.Digits
            N10(d0, a10, a9, a8, a7, a6, a5, a4, a3, a2)

        static member inline (+==) (l: N10<'ld10, 'ld9, 'ld8, 'ld7, 'ld6, 'ld5, 'ld4, 'ld3, 'ld2, 'ld1>, r:N10<'rd10, 'rd9, 'rd8, 'rd7, 'rd6, 'rd5, 'rd4, 'rd3, 'rd2, 'rd1>) =
            let (a10, a9, a8, a7, a6, a5, a4, a3, a2, a1) = l.Digits
            let (b10, b9, b8, b7, b6, b5, b4, b3, b2, b1) = r.Digits

            (a10 +=== b10) * (a9 +=== b9) * (a8 +=== b8) * (a7 +=== b7) * (a6 +=== b6) * (a5 +=== b5) * (a4 +=== b4) * (a3 +=== b3) * (a2 +=== b2) * (a1 +=== b1)

        static member inline (+!=) (l: N10<'ld10, 'ld9, 'ld8, 'ld7, 'ld6, 'ld5, 'ld4, 'ld3, 'ld2, 'ld1>, r:N10<'rd10, 'rd9, 'rd8, 'rd7, 'rd6, 'rd5, 'rd4, 'rd3, 'rd2, 'rd1>) =
            !! (l +== r)
        
        static member inline (+.) (l: N10<'ld10, 'ld9, 'ld8, 'ld7, 'ld6, 'ld5, 'ld4, 'ld3, 'ld2, 'ld1>, r:N10<'rd10, 'rd9, 'rd8, 'rd7, 'rd6, 'rd5, 'rd4, 'rd3, 'rd2, 'rd1>) =
            let (a10, a9, a8, a7, a6, a5, a4, a3, a2, a1) = l.Digits
            let (b10, b9, b8, b7, b6, b5, b4, b3, b2, b1) = r.Digits

            let inline (+++) a (b, c) =
                let carry1, rem1 = a + b
                let carry2, rem2 = rem1 + c
                let _, carry3 = carry1 + carry2
                carry3, rem2

            let carry1, rem1 = a1 + b1
            let carry2, rem2 = a2 +++ (b2, carry1)
            let carry3, rem3 = a3 +++ (b3, carry2)
            let carry4, rem4 = a4 +++ (b4, carry3)
            let carry5, rem5 = a5 +++ (b5, carry4)
            let carry6, rem6 = a6 +++ (b6, carry5)
            let carry7, rem7 = a7 +++ (b7, carry6)
            let carry8, rem8 = a8 +++ (b8, carry7)
            let carry9, rem9 = a9 +++ (b9, carry8)
            let carry10, rem10 = a10 +++ (b10, carry9)
        
            carry10, N10(rem10, rem9, rem8, rem7, rem6, rem5, rem4, rem3, rem2, rem1)
    
        static member inline (+) (l: N10<'ld10, 'ld9, 'ld8, 'ld7, 'ld6, 'ld5, 'ld4, 'ld3, 'ld2, 'ld1>, r:N10<'rd10, 'rd9, 'rd8, 'rd7, 'rd6, 'rd5, 'rd4, 'rd3, 'rd2, 'rd1>) =
            let (o, n) = l +. r
            !?? o <?> (n, N10Overflow)

        static member inline (-) (a, b) = 
            let (u, n) = (!!! a) +. b
            !?? u <?> (!!! n, N10Underflow)

        static member inline (*) (a : N10<_, _, _, _, _, _, _, _, _, _>, b) =
            let (a10, a9, a8, a7, a6, a5, a4, a3, a2, a1) = a.Digits
       
            let b1 = b
            let b2 = !<<< b
            let b3 = !<<< (!<<< b)
            let b4 = !<<< (!<<< (!<<< b))
            let b5 = !<<< (!<<< (!<<< (!<<< b)))
            let b6 = !<<< (!<<< (!<<< (!<<< (!<<< b))))
            let b7 = !<<< (!<<< (!<<< (!<<< (!<<< (!<<< b)))))
            let b8 = !<<<(!<<< (!<<< (!<<< (!<<< (!<<< (!<<< b))))))
            let b9 = !<<< (!<<<(!<<< (!<<< (!<<< (!<<< (!<<< (!<<< b)))))))
            let b10 = !<<< (!<<< (!<<<(!<<< (!<<< (!<<< (!<<< (!<<< (!<<< b))))))))

            (a10 ++* b10) + (a9 ++* b9) + (a8 ++* b8) + (a7 ++* b7) + (a6 ++* b6) + (a5 ++* b5) + (a4 ++* b4) + (a3 ++* b3) + (a2 ++* b2) + (a1 ++* b1)
            
        static member inline (+>=) (l: N10<'ld10, 'ld9, 'ld8, 'ld7, 'ld6, 'ld5, 'ld4, 'ld3, 'ld2, 'ld1>, r:N10<'rd10, 'rd9, 'rd8, 'rd7, 'rd6, 'rd5, 'rd4, 'rd3, 'rd2, 'rd1>) =
            let (a10, a9, a8, a7, a6, a5, a4, a3, a2, a1) = l.Digits
            let (b10, b9, b8, b7, b6, b5, b4, b3, b2, b1) = r.Digits
        
            let inline (+>>) (a, b) c = (a ++* fst(b +>>> c), a ++* snd(b +>>> c))
        
            let c10, r10 = (d1, a10) +>> b10
            let c9, r9 = (c10, a9) +>> b9
            let c8, r8 = (c9, a8) +>> b8
            let c7, r7 = (c8, a7) +>> b7
            let c6, r6 = (c7, a6) +>> b6
            let c5, r5 = (c6, a5) +>> b5
            let c4, r4 = (c5, a4) +>> b4
            let c3, r3 = (c4, a3) +>> b3
            let c2, r2 = (c3, a2) +>> b2
            let c1, r1 = (c2, a1) +>> b1
        
            isZero (r10, r9, r8, r7, r6, r5, r4, r3, r2, r1)
           
        static member inline (+>) (l, r) = (l +>= r) * (!! (l +== r))
            
        static member inline (+<) (l, r) = (!! (l +>= r))

        static member inline (+<=) (l, r) = (!! (l +> r))

        static member inline (+@<<) (l, r) = (l +< r) <?> (IndexInRange(r), l)
        
        static member inline (+@<) (l, r) = (l +< r) <?> (LessThan(r), l)
        
        static member inline (+@<=) (l , r) = (l +<= r) <?> (LessThanOrEqual(r), l)

        static member inline (+@>) (l, r) = (l +> r) <?> (GreaterThan(r), l)

        static member inline (+@>=) (l, r) = (l +>= r) <?> (GreaterThanOrEqual(r), l)

        static member inline (+@==) (l, r) = (l +== r) <?> (Equal(r), l)

        static member inline (+@!=) (l, r) = (l +!= r) <?> (NotEqual(r), l)

        static member inline op_Explicit (l: N10<'ld10, 'ld9, 'ld8, 'ld7, 'ld6, 'ld5, 'ld4, 'ld3, 'ld2, 'ld1>) : int = Checked.int(let n = l :> Number in n.IntVal)

        static member inline op_Explicit (l: N10<'ld10, 'ld9, 'ld8, 'ld7, 'ld6, 'ld5, 'ld4, 'ld3, 'ld2, 'ld1>) : uint32 = Checked.uint32(let n = l :> Number in n.IntVal)

        static member inline op_Explicit (l: N10<'ld10, 'ld9, 'ld8, 'ld7, 'ld6, 'ld5, 'ld4, 'ld3, 'ld2, 'ld1>) : uint64 = let n = l :> Number in n.UIntVal
        
        static member inline (+) (l: N10<'ld10, 'ld9, 'ld8, 'ld7, 'ld6, 'ld5, 'ld4, 'ld3, 'ld2, 'ld1>, r:int) = Checked.int((int) l +  r)

        static member inline (+) (l: int, r: N10<'ld10, 'ld9, 'ld8, 'ld7, 'ld6, 'ld5, 'ld4, 'ld3, 'ld2, 'ld1>) = Checked.int(l + (int) r)

        static member inline (+) (l: N10<'ld10, 'ld9, 'ld8, 'ld7, 'ld6, 'ld5, 'ld4, 'ld3, 'ld2, 'ld1>, r:uint32) = Checked.uint32((uint32) l +  r)

        static member inline (+) (l: uint32, r: N10<'ld10, 'ld9, 'ld8, 'ld7, 'ld6, 'ld5, 'ld4, 'ld3, 'ld2, 'ld1>) = Checked.uint32(l +  (uint32) r)

        static member inline (+) (l: N10<'ld10, 'ld9, 'ld8, 'ld7, 'ld6, 'ld5, 'ld4, 'ld3, 'ld2, 'ld1>, r:uint64) = Checked.uint64((uint64) l + r)

        static member inline (+) (l: uint64, r: N10<'ld10, 'ld9, 'ld8, 'ld7, 'ld6, 'ld5, 'ld4, 'ld3, 'ld2, 'ld1>) = Checked.uint64(l + (uint64) r)

        static member inline (-) (l: N10<'ld10, 'ld9, 'ld8, 'ld7, 'ld6, 'ld5, 'ld4, 'ld3, 'ld2, 'ld1>, r:int) = Checked.int((int) l -  r)

        static member inline (-) (l: int, r: N10<'ld10, 'ld9, 'ld8, 'ld7, 'ld6, 'ld5, 'ld4, 'ld3, 'ld2, 'ld1>) = Checked.int(l - (int) r)

        static member inline (-) (l: N10<'ld10, 'ld9, 'ld8, 'ld7, 'ld6, 'ld5, 'ld4, 'ld3, 'ld2, 'ld1>, r:uint32) = Checked.uint32((uint32) l -  r)

        static member inline (-) (l: uint32, r: N10<'ld10, 'ld9, 'ld8, 'ld7, 'ld6, 'ld5, 'ld4, 'ld3, 'ld2, 'ld1>) = Checked.uint32(l -  (uint32) r)

        static member inline (-) (l: N10<'ld10, 'ld9, 'ld8, 'ld7, 'ld6, 'ld5, 'ld4, 'ld3, 'ld2, 'ld1>, r:uint64) = Checked.uint64((uint64) l - r)

        static member inline (-) (l: uint64, r: N10<'ld10, 'ld9, 'ld8, 'ld7, 'ld6, 'ld5, 'ld4, 'ld3, 'ld2, 'ld1>) = Checked.uint64(l - (uint64) r)

        static member inline (*) (l: N10<'ld10, 'ld9, 'ld8, 'ld7, 'ld6, 'ld5, 'ld4, 'ld3, 'ld2, 'ld1>, r:int) = Checked.int((int) l *  r)

        static member inline (*) (l: int, r: N10<'ld10, 'ld9, 'ld8, 'ld7, 'ld6, 'ld5, 'ld4, 'ld3, 'ld2, 'ld1>) = Checked.int(l * (int) r)

        static member inline (*) (l: N10<'ld10, 'ld9, 'ld8, 'ld7, 'ld6, 'ld5, 'ld4, 'ld3, 'ld2, 'ld1>, r:uint32) = Checked.uint32((uint32) l *  r)

        static member inline (*) (l: uint32, r: N10<'ld10, 'ld9, 'ld8, 'ld7, 'ld6, 'ld5, 'ld4, 'ld3, 'ld2, 'ld1>) = Checked.uint32(l *  (uint32) r)

        static member inline (*) (l: N10<'ld10, 'ld9, 'ld8, 'ld7, 'ld6, 'ld5, 'ld4, 'ld3, 'ld2, 'ld1>, r:uint64) = Checked.uint64((uint64) l * r)

        static member inline (*) (l: uint64, r: N10<'ld10, 'ld9, 'ld8, 'ld7, 'ld6, 'ld5, 'ld4, 'ld3, 'ld2, 'ld1>) = Checked.uint64(l * (uint64) r)

    and N1<'d1 when 'd1:>Base10Digit>() = inherit N10<Base10.``0``, Base10.``0``, Base10.``0``, Base10.``0``, Base10.``0``, Base10.``0``, Base10.``0``, Base10.``0``, Base10.``0``, 'd1>()
    and N2<'d2, 'd1 when 'd2:> Base10Digit and 'd1:>Base10Digit> = N10<Base10.``0``, Base10.``0``, Base10.``0``, Base10.``0``, Base10.``0``, Base10.``0``, Base10.``0``, Base10.``0``, 'd2, 'd1>
    and N3<'d3, 'd2, 'd1 when 'd3 :> Base10Digit and 'd2 :> Base10Digit and 'd1 :> Base10Digit> = N10<Base10.``0``, Base10.``0``, Base10.``0``, Base10.``0``, Base10.``0``, Base10.``0``, Base10.``0``, 'd3, 'd2, 'd1>
    and N4<'d4, 'd3, 'd2, 'd1 when 'd4 :> Base10Digit and 'd3 :> Base10Digit and 'd2 :> Base10Digit and 'd1 :> Base10Digit> = 
               N10<Base10.``0``, Base10.``0``, Base10.``0``, Base10.``0``, Base10.``0``, Base10.``0``, 'd4, 'd3, 'd2, 'd1>
    and N5<'d5, 'd4, 'd3, 'd2, 'd1 when 'd5:> Base10Digit and 'd4 :> Base10Digit and 'd3 :> Base10Digit and 'd2 :> Base10Digit and 'd1 :> Base10Digit> = 
               N10<Base10.``0``, Base10.``0``, Base10.``0``, Base10.``0``, Base10.``0``, 'd5, 'd4, 'd3, 'd2, 'd1>
    and N6<'d6, 'd5, 'd4, 'd3, 'd2, 'd1 when 'd6 :> Base10Digit and 'd5:> Base10Digit and 'd4 :> Base10Digit and 'd3 :> Base10Digit and 'd2 :> Base10Digit and 'd1 :> Base10Digit> = 
               N10<Base10.``0``, Base10.``0``, Base10.``0``, Base10.``0``, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>
    and N7<'d7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1 when 'd7 :> Base10Digit and 'd6 :> Base10Digit and 'd5:> Base10Digit and 'd4 :> Base10Digit and 'd3 :> Base10Digit and 'd2 :> Base10Digit and 'd1 :> Base10Digit> = 
               N10<Base10.``0``, Base10.``0``, Base10.``0``, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>
    and N8<'d8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1 when 'd8 :> Base10Digit and 'd7 :> Base10Digit and 'd6 :> Base10Digit and 'd5:> Base10Digit and 'd4 :> Base10Digit and 'd3 :> Base10Digit and 'd2 :> Base10Digit and 'd1 :> Base10Digit> = 
               N10<Base10.``0``, Base10.``0``, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>
    and N9<'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1 when 'd9 :> Base10Digit and 'd8 :> Base10Digit and 'd7 :> Base10Digit and 'd6 :> Base10Digit and 'd5:> Base10Digit and 'd4 :> Base10Digit and 'd3 :> Base10Digit and 'd2 :> Base10Digit and 'd1 :> Base10Digit> = 
               N10<Base10.``0``, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>
    
    let inline min(l:'dim0) (r:'dim1) = (l +< r) <?> (l, r)

    let inline max(l:'dim0) (r:'dim1) = (l +> r) <?> (l, r)