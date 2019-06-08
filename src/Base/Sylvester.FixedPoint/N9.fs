namespace Sylvester

[<AutoOpen>]
module N9 = 

    type N9Overflow = N9Overflow
    
    type N9Underflow = N9Underflow

    type N9<'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1 when 'd9 :> Base10Digit and 'd8 :> Base10Digit and 'd7 :> Base10Digit and 'd6 :> Base10Digit
        and 'd5 :> Base10Digit and 'd4 :> Base10Digit and 'd3 :> Base10Digit and 'd2 :> Base10Digit and 'd1 :> Base10Digit> 
        (n9:'d9, n8:'d8, n7:'d7, n6:'d6, n5:'d5, n4:'d4, n3:'d3, n2:'d2, n1:'d1)  = 
    
        member x.Digits = (n9, n8, n7, n6, n5, n4, n3, n2, n1)

        member x.Digit1 = n1

        member x.Digit2 = n2

        member x.Digit3 = n3

        member x.Digit4 = n4

        member x.Digit5 = n5

        member x.Digit6 = n6

        member x.Digit7 = n7

        member x.Digit8 = n8

        member x.Digit9 = n9

        new() = N9(digit<'d9>(), digit<'d8>(), digit<'d7>(), digit<'d6>(), digit<'d5>(), digit<'d4>(), digit<'d3>(), digit<'d2>(), digit<'d1>())

        static member Zero = N9(d0, d0, d0, d0, d0, d0, d0, d0, d0)

        static member inline (!?) (l: N9<'ld9, 'ld8, 'ld7, 'ld6, 'ld5, 'ld4, 'ld3, 'ld2, 'ld1>) =
            let (a9, a8, a7, a6, a5, a4, a3, a2, a1) = l.Digits
            (!!!! a9) * (!!!! a8) * (!!!! a7) * (!!!! a6) * (!!!! a5) * (!!!! a4) * (!!!! a3) * (!!!! a2) * (!!!! a1)

        static member inline (!!?) (l: N9<'ld9, 'ld8, 'ld7, 'ld6, 'ld5, 'ld4, 'ld3, 'ld2, 'ld1>) = !!(!? l)
    
        static member inline (!!!) (a: N9<_, _, _, _, _, _, _, _, _>) =
            let (a9, a8, a7, a6, a5, a4, a3, a2, a1) = a.Digits
            N9(!! a9, !! a8, !!a7, !!a6, !! a5, !! a4, !! a3, !! a2, !! a1)

        static member inline (!<<<) (a: N9<_, _, _, _, _, _, _, _, _>) =
            let (_, a8, a7, a6, a5, a4, a3, a2, a1) = a.Digits
            N9(a8, a7, a6, a5, a4, a3, a2, a1, d0)

        static member inline (!>>>) (a: N9<_, _, _, _, _, _, _, _, _>) =
            let (a9, a8, a7, a6, a5, a4, a3, a2, _) = a.Digits
            N9(d0, a9, a8, a7, a6, a5, a4, a3, a2)

        static member inline (+==) (l: N9<'ld9, 'ld8, 'ld7, 'ld6, 'ld5, 'ld4, 'ld3, 'ld2, 'ld1>, r:N9<'rd9, 'rd8, 'rd7, 'rd6, 'rd5, 'rd4, 'rd3, 'rd2, 'rd1>) =
            let (a9, a8, a7, a6, a5, a4, a3, a2, a1) = l.Digits
            let (b9, b8, b7, b6, b5, b4, b3, b2, b1) = r.Digits

            (a9 +=== b9) * (a8 +=== b8) * (a7 +=== b7) * (a6 +=== b6) * (a5 +=== b5) * (a4 +=== b4) * (a3 +=== b3) * (a2 +=== b2) * (a1 +=== b1)

        static member inline (+!=) (l: N9<'ld9, 'ld8, 'ld7, 'ld6, 'ld5, 'ld4, 'ld3, 'ld2, 'ld1>, r:N9<'rd9, 'rd8, 'rd7, 'rd6, 'rd5, 'rd4, 'rd3, 'rd2, 'rd1>) =
            !! (l +== r)

        static member inline (+.) (l: N9<'ld9, 'ld8, 'ld7, 'ld6, 'ld5, 'ld4, 'ld3, 'ld2, 'ld1>, r:N9<'rd9, 'rd8, 'rd7, 'rd6, 'rd5, 'rd4, 'rd3, 'rd2, 'rd1>) =
            let (a9, a8, a7, a6, a5, a4, a3, a2, a1) = l.Digits
            let (b9, b8, b7, b6, b5, b4, b3, b2, b1) = r.Digits

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
        
            carry9, N9(rem9, rem8, rem7, rem6, rem5, rem4, rem3, rem2, rem1)
    
        static member inline (+) (l: N9<'ld9, 'ld8, 'ld7, 'ld6, 'ld5, 'ld4, 'ld3, 'ld2, 'ld1>, r:N9<'rd9, 'rd8, 'rd7, 'rd6, 'rd5, 'rd4, 'rd3, 'rd2, 'rd1>) =
            let (o, n) = l +. r
            !!!! o <?> (n, N9Overflow)

        static member inline (-) (a, b) = 
            let (u, n) = (!!! a) +. b
            !!!! u <?> (!!! n, N9Underflow)

        static member inline (*) (a : N9<_, _, _, _, _, _, _, _, _>, b) =
            let (a9, a8, a7, a6, a5, a4, a3, a2, a1) = a.Digits
       
            let b1 = b
            let b2 = !<<< b
            let b3 = !<<< (!<<< b)
            let b4 = !<<< (!<<< (!<<< b))
            let b5 = !<<< (!<<< (!<<< (!<<< b)))
            let b6 = !<<< (!<<< (!<<< (!<<< (!<<< b))))
            let b7 = !<<< (!<<< (!<<< (!<<< (!<<< (!<<< b)))))
            let b8 = !<<<(!<<< (!<<< (!<<< (!<<< (!<<< (!<<< b))))))
            let b9 = !<<< (!<<<(!<<< (!<<< (!<<< (!<<< (!<<< (!<<< b)))))))

            (a9 ++* b9) + (a8 ++* b8) + (a7 ++* b7) + (a6 ++* b6) + (a5 ++* b5) + (a4 ++* b4) + (a3 ++* b3) + (a2 ++* b2) + (a1 ++* b1)
            

        static member inline (+>) (l: N9<'ld9, 'ld8, 'ld7, 'ld6, 'ld5, 'ld4, 'ld3, 'ld2, 'ld1>, r:N9<'rd9, 'rd8, 'rd7, 'rd6, 'rd5, 'rd4, 'rd3, 'rd2, 'rd1>) =
            let (a9, a8, a7, a6, a5, a4, a3, a2, a1) = l.Digits
            let (b9, b8, b7, b6, b5, b4, b3, b2, b1) = r.Digits
        
            let inline (+>>) (a, b) c = (a ++* fst(b +>>> c), a ++* snd(b +>>> c))
        
            let inline _isZero (x1, x2, x3, x4, x5, x6, x7, x8, x9)  = 
                (!!!! x9) * (!!!! x8) * (!!!! x7) * (!!!! x6) * (!!!! x5) * (!!!! x4) * (!!!! x3) * (!!!! x2) * (!!!! x1)

            let c9, r9 = (d1, a9) +>> b9
            let c8, r8 = (c9, a8) +>> b8
            let c7, r7 = (c8, a7) +>> b7
            let c6, r6 = (c7, a6) +>> b6
            let c5, r5 = (c6, a5) +>> b5
            let c4, r4 = (c5, a4) +>> b4
            let c3, r3 = (c4, a3) +>> b3
            let c2, r2 = (c3, a2) +>> b2
            let c1, r1 = (c2, a1) +>> b1
        
            (!! (l +== r)) * _isZero (r9, r8, r7, r6, r5, r4, r3, r2, r1)
            
        static member inline (+<) (l: N9<'ld9, 'ld8, 'ld7, 'ld6, 'ld5, 'ld4, 'ld3, 'ld2, 'ld1>, r:N9<'rd9, 'rd8, 'rd7, 'rd6, 'rd5, 'rd4, 'rd3, 'rd2, 'rd1>) =
            (!! (l +== r)) * (!! (l +> r))

        static member IsZero (_: N9<'ld9, 'ld8, 'ld7, 'ld6, 'ld5, 'ld4, 'ld3, 'ld2, 'ld1>  
            when 'ld9 :> ZeroDigit and 'ld8 :> ZeroDigit and 'ld7 :> ZeroDigit and 'ld6 :> ZeroDigit and 'ld5 :> ZeroDigit and 'ld4 :> ZeroDigit
            and 'ld3 :> ZeroDigit and 'ld2 :> ZeroDigit and 'ld1 :> ZeroDigit) = True
   
    type N0 = N9<_0, _0, _0, _0, _0, _0, _0, _0, _0>

    type N1<'d1 when 'd1:>Base10Digit> = N9<_0, _0, _0, _0, _0, _0, _0, _0, 'd1>
    
    type N2<'d2, 'd1 when 'd2:> Base10Digit and 'd1:>Base10Digit> = N9<_0, _0, _0, _0, _0, _0, _0, 'd2, 'd1>

    type N3<'d3, 'd2, 'd1 when 'd3 :> Base10Digit and 'd2 :> Base10Digit and 'd1 :> Base10Digit> = N9<_0, _0, _0, _0, _0, _0, 'd3, 'd2, 'd1>

    type N4<'d4, 'd3, 'd2, 'd1 when 'd4 :> Base10Digit and 'd3 :> Base10Digit and 'd2 :> Base10Digit and 'd1 :> Base10Digit> = 
        N9<_0, _0, _0, _0, _0, 'd4, 'd3, 'd2, 'd1>

    type N5<'d5, 'd4, 'd3, 'd2, 'd1 when 'd5:> Base10Digit and 'd4 :> Base10Digit and 'd3 :> Base10Digit and 'd2 :> Base10Digit and 'd1 :> Base10Digit> = 
        N9<_0, _0, _0, _0, 'd5, 'd4, 'd3, 'd2, 'd1>

   
    let zero = N9(d0, d0, d0, d0, d0, d0, d0, d0, d0)

    let one = N9(d0, d0, d0, d0, d0, d0, d0, d0, d1)

    let two = N9(d0, d0, d0, d0, d0, d0, d0, d0, d2)

    let three = N9(d0, d0, d0, d0, d0, d0, d0, d0, d3)

    let four = N9(d0, d0, d0, d0, d0, d0, d0, d0, d4)

    let five = N9(d0, d0, d0, d0, d0, d0, d0, d0, d5)

    let six = N9(d0, d0, d0, d0, d0, d0, d0, d0, d6)

    let seven = N9(d0, d0, d0, d0, d0, d0, d0, d0, d7)

    let eight = N9(d0, d0, d0, d0, d0, d0, d0, d0, d8)

    let nine = N9(d0, d0, d0, d0, d0, d0, d0, d0, d8)

    let ten = N9(d0, d0, d0, d0, d0, d0, d0, d1, d0)

    let twenty = ten * two

    let hundred = N9(d0, d0, d0, d0, d0, d0, d1, d0, d0)

    let thousand = N9(d0, d0, d0, d0, d0, d1, d0, d0, d0)

    let million = N9(d0, d0, d1, d0, d0, d0, d0, d0, d0)

    let _N1(n1: #Base10Digit) = N9(d0, d0, d0, d0, d0, d0, d0, d0, n1)
    
    let _N2(n2: #Base10Digit, n1: #Base10Digit) = N9(d0, d0, d0, d0, d0, d0, d0, n2, n1)
     
    let _N3(n3: #Base10Digit, n2: #Base10Digit, n1: #Base10Digit) =
        N9(d0, d0, d0, d0, d0, d0, n3, n2, n1)
        
    let _N4(n4: #Base10Digit, n3: #Base10Digit, n2: #Base10Digit, n1: #Base10Digit) =
        N9(d0, d0, d0, d0, d0, n4, n3, n2, n1)
        
    let _N5(n5: #Base10Digit, n4: #Base10Digit, n3: #Base10Digit, n2: #Base10Digit, n1: #Base10Digit) =
        N9(d0, d0, d0, d0, n5, n4, n3, n2, n1)

    


    

