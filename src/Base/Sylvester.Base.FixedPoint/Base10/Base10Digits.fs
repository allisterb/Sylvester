namespace Sylvester
#nowarn "0077"

module Base10Digits =
  type Base10Digit = interface end
  
  type ZeroDigit = interface inherit Base10Digit end

  type NonZeroDigit = interface inherit Base10Digit end

  type MultipleOf2Digit = interface inherit Base10Digit end

  type MultipleOf3Digit = interface inherit Base10Digit end

  type PowerOf2Digit = interface inherit MultipleOf2Digit end

  type PowerOf3Digit = interface inherit MultipleOf3Digit end

  type PowerOf5Digit = interface inherit Base10Digit end

  type SquareDigit = interface inherit Base10Digit end

  type PrimeDigit = interface inherit Base10Digit end

  type I0 = interface inherit ZeroDigit end

  type I1 = interface inherit NonZeroDigit end

  type I2 = interface inherit NonZeroDigit inherit PowerOf2Digit inherit PrimeDigit end

  type I3 = interface inherit NonZeroDigit inherit PowerOf3Digit inherit PrimeDigit end

  type I4 = interface inherit NonZeroDigit inherit PowerOf2Digit inherit SquareDigit end

  type I5 = interface inherit NonZeroDigit inherit PowerOf5Digit inherit PrimeDigit end

  type I6 = interface inherit NonZeroDigit inherit MultipleOf2Digit inherit MultipleOf3Digit end

  type I7 = interface inherit NonZeroDigit inherit PrimeDigit end

  type I8 = interface inherit NonZeroDigit inherit PowerOf2Digit end

  type I9 = interface inherit NonZeroDigit inherit PowerOf3Digit inherit SquareDigit end

  type _0() = interface I0
  type _1() = interface I1
  type _2() = interface I2
  type _3() = interface I3
  type _4() = interface I4
  type _5() = interface I5
  type _6() = interface I6
  type _7() = interface I7
  type _8() = interface I8
  type _9() = interface I9

  let D0 = _0()
  let D1 = _1()
  let D2 = _2()
  let D3 = _3()
  let D4 = _4()
  let D5 = _5()
  let D6 = _6()
  let D7 = _7()
  let D8 = _8()
  let D9 = _9()
  

  //type N5<'d1, 'd2, 'd3 when 'd1 :> Base10Digit and 'd2 :>Base10Digit and 'd3 :> Base10Digit>(d5: 'd1, d4: 'd2, d6: 'd3) = class end

  //type Even<'d1, 'd0 when 'd1 :> Base10Digit and 'd0 :> PowerOf2Digit> = Even of N2<'d1, 'd0>

  type _0 with
    static member inline (!!) = D9
    static member inline (+) (_:_0, _:_0) = (D0, D0)
    static member inline (+) (_:_0, _:_1) = (D0, D1)
    static member inline (+) (_:_0, _:_2) = (D0, D2)
    static member inline (+) (_:_0, _:_3) = (D0, D3)
    static member inline (+) (_:_0, _:_4) = (D0, D4)
    static member inline (+) (_:_0, _:_5) = (D0, D5)
    static member inline (+) (_:_0, _:_6) = (D0, D6)
    static member inline (+) (_:_0, _:_7) = (D0, D7)
    static member inline (+) (_:_0, _:_8) = (D0, D8)
    static member inline (+) (_:_0, _:_9) = (D0, D9)
    
  type _1 with
    static member inline (!!) = _8
    static member inline (+) (_:_1, _:_0) = (D1, D1)
    static member inline (+) (_:_1, _:_1) = (D0, D2)
    static member inline (+) (_:_1, _:_2) = (D0, D3)
    static member inline (+) (_:_1, _:_3) = (D0, D4)
    static member inline (+) (_:_1, _:_4) = (D0, D5)
    static member inline (+) (_:_1, _:_5) = (D0, D6)
    static member inline (+) (_:_1, _:_6) = (D0, D7)
    static member inline (+) (_:_1, _:_7) = (D0, D8)
    static member inline (+) (_:_1, _:_8) = (D0, D9)
    static member inline (+) (_:_1, _:_9) = (D1, D0)

  type _2 with
    static member inline (!!) = D7
    static member inline (+) (_:_2, _:_0) = (D0, D2)
    static member inline (+) (_:_2, _:_1) = (D0, D3)
    static member inline (+) (_:_2, _:_2) = (D0, D4)
    static member inline (+) (_:_2, _:_3) = (D0, D5)
    static member inline (+) (_:_2, _:_4) = (D0, D6)
    static member inline (+) (_:_2, _:_5) = (D0, D7)
    static member inline (+) (_:_2, _:_6) = (D0, D8)
    static member inline (+) (_:_2, _:_7) = (D0, D9)
    static member inline (+) (_:_2, _:_8) = (D1, D0)
    static member inline (+) (_:_2, _:_9) = (D1, D1)

  type _3 with
    static member inline (!!) = D6
    static member inline (+) (_:_3, _:_0) = (D0, D3)
    static member inline (+) (_:_3, _:_1) = (D0, D4)
    static member inline (+) (_:_3, _:_2) = (D0, D5)
    static member inline (+) (_:_3, _:_3) = (D0, D6)
    static member inline (+) (_:_3, _:_4) = (D0, D7)
    static member inline (+) (_:_3, _:_5) = (D0, D8)
    static member inline (+) (_:_3, _:_6) = (D0, D9)
    static member inline (+) (_:_3, _:_7) = (D1, D0)
    static member inline (+) (_:_3, _:_8) = (D1, D1)
    static member inline (+) (_:_3, _:_9) = (D1, D2)

  (*
  type _:_4 with
    static member inline (!!) = D5
    static member inline (+) (_:_4, _:_0) = (_:_0, _:_4)
    static member inline (+) (_:_4, _:_1) = (_:_0, _:_5)
    static member inline (+) (_:_4, _:_2) = (_:_0, _:_6)
    static member inline (+) (_:_4, _:_3) = (_:_0, _:_7)
    static member inline (+) (_:_4, _:_4) = (_:_0, _:_8)
    static member inline (+) (_:_4, _:_5) = (_:_0, _:_9)
    static member inline (+) (_:_4, _:_6) = (_:_1, _:_0)
    static member inline (+) (_:_4, _:_7) = (_:_1, _:_1)
    static member inline (+) (_:_4, _:_8) = (_:_1, _:_2)
    static member inline (+) (_:_4, _:_9) = (_:_1, _:_3)

  type _:_5 with
    static member inline (!!) _:_5 = _:_4
    static member inline (+) (_:_5, _:_0) = (_:_0, _:_5)
    static member inline (+) (_:_5, _:_1) = (_:_0, _:_6)
    static member inline (+) (_:_5, _:_2) = (_:_0, _:_7)
    static member inline (+) (_:_5, _:_3) = (_:_0, _:_8)
    static member inline (+) (_:_5, _:_4) = (_:_0, _:_9)
    static member inline (+) (_:_5, _:_5) = (_:_1, _:_0)
    static member inline (+) (_:_5, _:_6) = (_:_1, _:_1)
    static member inline (+) (_:_5, _:_7) = (_:_1, _:_2)
    static member inline (+) (_:_5, _:_8) = (_:_1, _:_3)
    static member inline (+) (_:_5, _:_9) = (_:_1, _:_4)

  type _:_6 with
    static member inline (!!) _:_6 = _:_3
    static member inline (+) (_:_6, _:_0) = (_:_0, _:_6)
    static member inline (+) (_:_6, _:_1) = (_:_0, _:_7)
    static member inline (+) (_:_6, _:_2) = (_:_0, _:_8)
    static member inline (+) (_:_6, _:_3) = (_:_0, _:_9)
    static member inline (+) (_:_6, _:_4) = (_:_1, _:_0)
    static member inline (+) (_:_6, _:_5) = (_:_1, _:_1)
    static member inline (+) (_:_6, _:_6) = (_:_1, _:_2)
    static member inline (+) (_:_6, _:_7) = (_:_1, _:_3)
    static member inline (+) (_:_6, _:_8) = (_:_1, _:_4)
    static member inline (+) (_:_6, _:_9) = (_:_1, _:_5)

  type _:_7 with
    static member inline (!!) _:_7 = _:_2
    static member inline (+) (_:_7, _:_0) = (_:_0, _:_7)
    static member inline (+) (_:_7, _:_1) = (_:_0, _:_8)
    static member inline (+) (_:_7, _:_2) = (_:_0, _:_9)
    static member inline (+) (_:_7, _:_3) = (_:_1, _:_0)
    static member inline (+) (_:_7, _:_4) = (_:_1, _:_1)
    static member inline (+) (_:_7, _:_5) = (_:_1, _:_2)
    static member inline (+) (_:_7, _:_6) = (_:_1, _:_3)
    static member inline (+) (_:_7, _:_7) = (_:_1, _:_4)
    static member inline (+) (_:_7, _:_8) = (_:_1, _:_5)
    static member inline (+) (_:_7, _:_9) = (_:_1, _:_6)

  type _:_8 with
    static member inline (!!) _:_8 = _:_1
    static member inline (+) (_:_8, _:_0) = (_:_0, _:_8)
    static member inline (+) (_:_8, _:_1) = (_:_0, _:_9)
    static member inline (+) (_:_8, _:_2) = (_:_1, _:_0)
    static member inline (+) (_:_8, _:_3) = (_:_1, _:_1)
    static member inline (+) (_:_8, _:_4) = (_:_1, _:_2)
    static member inline (+) (_:_8, _:_5) = (_:_1, _:_3)
    static member inline (+) (_:_8, _:_6) = (_:_1, _:_4)
    static member inline (+) (_:_8, _:_7) = (_:_1, _:_5)
    static member inline (+) (_:_8, _:_8) = (_:_1, _:_6)
    static member inline (+) (_:_8, _:_9) = (_:_1, _:_7)

  type _:_9 with
    static member inline (!!) _:_9 = _:_0
    static member inline (+) (_:_9, _:_0) = (_:_0, _:_9)
    static member inline (+) (_:_9, _:_1) = (_:_1, _:_0)
    static member inline (+) (_:_9, _:_2) = (_:_1, _:_1)
    static member inline (+) (_:_9, _:_3) = (_:_1, _:_2)
    static member inline (+) (_:_9, _:_4) = (_:_1, _:_3)
    static member inline (+) (_:_9, _:_5) = (_:_1, _:_4)
    static member inline (+) (_:_9, _:_6) = (_:_1, _:_5)
    static member inline (+) (_:_9, _:_7) = (_:_1, _:_6)
    static member inline (+) (_:_9, _:_8) = (_:_1, _:_7)
    static member inline (+) (_:_9, _:_9) = (_:_1, _:_8)
  *)
  type _0 with static member inline (.*) (_:_0, nn : ^nn) = ( ^nn : (static member Zero : ^zero) ())
  type _1 with static member inline (.*) (_:_1, nn) = nn
  type _2 with static member inline (.*) (_:_2, nn) = nn + nn
  type _3 with static member inline (.*) (_:_3, nn) = nn + nn + nn
  type _4 with static member inline (.*) (_:_4, nn) = nn + nn + nn + nn
  
  
  
  //type _:_5 with static member inline (.*) (_:_5, nn) = nn + nn + nn + nn + nn
  //type _:_6 with static member inline (.*) (_:_6, nn) = nn + nn + nn + nn + nn + nn
  //type _:_7 with static member inline (.*) (_:_7, nn) = nn + nn + nn + nn + nn + nn + nn
  //type _:_8 with static member inline (.*) (_:_8, nn) = nn + nn + nn + nn + nn + nn + nn + nn
  //type _:_9 with static member inline (.*) (_:_9, nn) = nn + nn + nn + nn + nn + nn + nn + nn + nn

  