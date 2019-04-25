namespace Sylvester

module Base10 =
  open Base10Digits

  type N5d<'a, 'b, 'c, 'd, 'e> = N5d of 'a * 'b * 'c * 'd * 'e with
    static member Zero = N5d(D0, D0, D0, D0, D0)
    static member One = N5d(D0, D0, D0, D0, D1)

    static member inline (+.) (N5d(a5, a4, a3, a2, a1), N5d(b5, b4, b3, b2, b1)) =
      let inline (+++) a (b, c) =
        let cc, r1 = a + b
        let cc1, r2 = r1 + c
        let D0, cc2 = cc + cc1
        cc2, r2

      let c2, r1 = a1 + b1
      let c3, r2 = a2 +++ (b2, c2)
      let c4, r3 = a3 +++ (b3, c3)
      let c5, r4 = a4 +++ (b4, c4)
      let c6, r5 = a5 +++ (b5, c5)
      c6, N5d(r5, r4, r3, r2, D0)
      

    static member inline (+) (a : N5d<_, _, _, _, _>, b) =
      snd (a +. b)

    static member inline (*) (N5d(a5, a4, a3, a2, a1), b) =
      let b1 = b
      let b2 = !<<< b
      let b3 = !<<< (!<<< b)
      let b4 = !<<< (!<<< (!<<< b))
      let b5 = !<<< (!<<< (!<<< (!<<< b)))
      (a5 .* b5) + (a4 .* b4) + (a3 .* b3) + (a2 .* b2) + (a1 .* b1)

    static member inline (!<<<) (N5d(a5, a4, a3, a2, a1)) =
      N5d(a4, a3, a2, a1, D0)

    static member inline (!>>>) (N5d(a5, a4, a3, a2, a1)) =
      N5d(D0, a5, a4, a3, a2)

    static member inline (!!) (N5d(a5, a4, a3, a2, a1)) : N5d<_, _, _, _, _> =
      N5d(!! a5, !! a4, !! a3, !! a2, !! a1)

    static member inline (-) (a, b) = !!((!! a) + b)

    //static member inline (!++) a = a + N5d(D0, D0, D0, D0, D1)
    //static member inline (!--) a = a - N5d(D0, D0, D0, D0, D1)


module Base10N16 =
  open Base10Digits
  
  type N16b<'d5, 'd4, 'd3, 'd2, 'd1 when 'd5 :> Base10Digit and 'd4 :> Base10Digit and 'd3 :> Base10Digit and 'd2 :> Base10Digit 
  and 'd1 :> Base10Digit> (n5:'d5, n4:'d4, n3:'d3, n2:'d2, n1:'d1)  = 
    
    member x.Digits = (n5, n4, n3, n2, n1)

    static member Zero = N16b(D0, D0, D0, D0, D0)
    static member One = N16b(D0, D0, D0, D0, D1)

    static member inline (+.) (l: N16b<'ld5, 'ld4, 'ld3, 'ld2, 'ld1>, r:N16b<'rd5, 'rd4, 'rd3, 'rd2, 'rd1>) =
    
        let (a5, a4, a3, a2, a1) = l.Digits
        let (b5, b4, b3, b2, b1) = r.Digits

        let inline (+++) a (b, c) =
            let cc, r1 = a + b
            let cc1, r2 = r1 + c
            let D0, cc2 = cc + cc1
            cc2, r2

        let c2, r1 = a1 + b1
        let c3, r2 = a2 +++ (b2, c2)
        let c4, r3 = a3 +++ (b3, c3)
        let c5, r4 = a4 +++ (b4, c4)
        let c6, r5 = a5 +++ (b5, c5)
        c6, N16b(r5, r4, r3, r2, D0)

    static member inline (+) (a : N16b<_, _, _, _, _>, b) =
      snd (a +. b)

   
        
#if false
  let s4444444444444444even = D3 + D4
  let testAdd = N5d(D0, D0, D4, D1, D2) + N5d(D0, D0, D3, D2, D4)
  let testAdd' = N5d(D0, D0, D3, D2, D4) + N5d(D0, D0, D4, D1, D2)
  //let testComp = !!! N5d(D0, D2, D1, D8, D5)
  //let testSub = N5d(D0, D2, D3, D2, D4) - N5d(D0, D0, D4, D1, D3)

  let testAdd1 = N5d(D0, D0, D3, D4, D2) + (N5d(D0, D0, D3, D2, D4) + N5d(D0, D0, D4, D1, D2))
  let testAdd1' = (N5d(D0, D0, D3, D4, D2) + N5d(D0, D0, D3, D2, D4)) + N5d(D0, D0, D4, D1, D2)

  let a = N5d(D0, D0, D0, D1, D4)
  let b = N5d(D0, D0, D0, D2, D3)
  let c = N5d(D0, D0, D0, D2, D1)

  let testMult = a * b
  let testMult' = b * a
  let testMultShift = (N5d(D0, D0, D0, D0, D2) * a) * N5d(D0, D0, D1, D0, D0)
  let testMultShift' = N5d(D0, D0, D0, D0, D2) * (a * N5d(D0, D0, D1, D0, D0))
  let testMultA = (a * b) * c
  let testMultA' = a * (b * c)
  let testMult11 = N5d(D0, D0, D0, D1, D1) * N5d(D0, D0, D0, D1, D1)
#endif
