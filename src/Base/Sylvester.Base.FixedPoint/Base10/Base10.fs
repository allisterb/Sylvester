namespace Sylvester

open Base10Digits

type N5<'d5, 'd4, 'd3, 'd2, 'd1 when 'd5 :> Base10Digit and 'd4 :> Base10Digit and 'd3 :> Base10Digit and 'd2 :> Base10Digit 
    and 'd1 :> Base10Digit> (n5:'d5, n4:'d4, n3:'d3, n2:'d2, n1:'d1)  = 
    
    member x.Digits = (n5, n4, n3, n2, n1)

    static member inline (+.) (l: N5<'ld5, 'ld4, 'ld3, 'ld2, 'ld1>, r:N5<'rd5, 'rd4, 'rd3, 'rd2, 'rd1>) =
    
        let (a5, a4, a3, a2, a1) = l.Digits
        let (b5, b4, b3, b2, b1) = r.Digits

        let inline (+++) a (b, c) =
            let carry1, rem1 = a + b
            let carry2, rem2 = rem1 + c
            let d0, carry3 = carry1 + carry2
            carry3, rem2

        let carry1, rem1 = a1 + b1
        let carry2, rem2 = a2 +++ (b2, carry1)
        let carry3, rem3 = a3 +++ (b3, carry2)
        let carry4, rem4 = a4 +++ (b4, carry3)
        let carry5, rem5 = a5 +++ (b5, carry4)
        carry5, N5(rem5, rem4, rem3, rem2, rem1)

    static member inline (+) (a , b) =
        snd (a +. b)

    static member inline (*) (a : N5<_, _, _, _, _>, b) =
        let (a5, a4, a3, a2, a1) = a.Digits
       
        let b1 = b
        let b2 = !<<< b
        let b3 = !<<< (!<<< b)
        let b4 = !<<< (!<<< (!<<< b))
        let b5 = !<<< (!<<< (!<<< (!<<< b)))
        (a5 .* b5) + (a4 .* b4) + (a3 .* b3) + (a2 .* b2) + (a1 .* b1)

    static member inline (!<<<) (a: N5<_, _, _, _, _>) =
        let (a5, a4, a3, a2, a1) = a.Digits
        N5(a4, a3, a2, a1, d0)

    static member inline (!>>>) (a: N5<_, _, _, _, _>) =
        let (a5, a4, a3, a2, a1) = a.Digits
        N5(d0, a5, a4, a3, a2)

    static member inline (!!!) (a: N5<_, _, _, _, _>) =
        let (a5, a4, a3, a2, a1) = a.Digits
        N5(!! a5, !! a4, !! a3, !! a2, !! a1)

    static member inline (-) (a, b) = !!!((!!! a) + b)

[<AutoOpen>]
module Base10 =     
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
        | 9 -> typedefof<_9>
        | _ -> failwith "Invalid digit."

    let getDigits (d:int) = [| for i in d.ToString() do yield System.Int32.Parse(i.ToString()) |> getDigit |] //Quick and dirty way to extract digits from number


    let Zero = N5(d0, d0, d0, d0, d0)
    let One = N5(d0, d0, d0, d0, d1)
    