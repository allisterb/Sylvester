namespace Sylvester

module Base10 =
  open Base10Digits
  
  let getDigit(n:int) =
        match n with
        | 0 -> typedefof<_0>
        | 1 -> typedefof<_1>
        | 2 -> typedefof<_2>
        | 3 -> typedefof<_3>
        | 4 -> typedefof<_4>
        | 5 -> typedefof<_5>
        | 6 -> typedefof<_6>
        | 7 -> typedefof<_7>
        | 8 -> typedefof<_8>
        | _ -> failwith "Invalid digit."

  let getDigits (d:int) =
    [| for i in d.ToString() do yield System.Int32.Parse(i.ToString()) |> getDigit |] //Quick and dirty way to extract digits from number

  type N5<'d5, 'd4, 'd3, 'd2, 'd1 when 'd5 :> Base10Digit and 'd4 :> Base10Digit and 'd3 :> Base10Digit and 'd2 :> Base10Digit 
  and 'd1 :> Base10Digit> (n5:'d5, n4:'d4, n3:'d3, n2:'d2, n1:'d1)  = 
    
    member x.Digits = (n5, n4, n3, n2, n1)

    static member Zero = N5(d0, d0, d0, d0, d0)
    static member One = N5(d0, d0, d0, d0, d1)

    static member inline (+.) (l: N5<'ld5, 'ld4, 'ld3, 'ld2, 'ld1>, r:N5<'rd5, 'rd4, 'rd3, 'rd2, 'rd1>) =
    
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
        c6, N5(r5, r4, r3, r2, d0)

    static member inline (+) (a : N5<_, _, _, _, _>, b) =
      snd (a +. b)

    