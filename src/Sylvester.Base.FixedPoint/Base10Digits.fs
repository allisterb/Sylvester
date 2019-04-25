namespace Sylvester
#nowarn "0077"

module Base10Digits =

  type Base10Digit = interface end
  
  type IsD0 = interface end
  
  type IsNotD0 = interface end
  
  type D0 = D0 with
    interface Base10Digit
    interface IsD0
  
  type D1 = D1 with
    interface Base10Digit
    interface IsNotD0
  
  type D2 = D2 with
    interface Base10Digit
    interface IsNotD0
  
  type D3 = D3 with
    interface Base10Digit
    interface IsNotD0
  
  type D4 = D4 with
    interface Base10Digit
    interface IsNotD0
  
  type D5 = D5 with
    interface Base10Digit
    interface IsNotD0
  
  type D6 = D6 with
    interface Base10Digit
    interface IsNotD0
  
  type D7 = D7 with
    interface Base10Digit
    interface IsNotD0
  
  type D8 = D8 with
    interface Base10Digit
    interface IsNotD0
  
  type D9 = D9 with
    interface Base10Digit
    interface IsNotD0

  type D0 with
    static member inline (!!) D0 = D9
    static member inline (+) (D0, D0) = (D0, D0)
    static member inline (+) (D0, D1) = (D0, D1)
    static member inline (+) (D0, D2) = (D0, D2)
    static member inline (+) (D0, D3) = (D0, D3)
    static member inline (+) (D0, D4) = (D0, D4)
    static member inline (+) (D0, D5) = (D0, D5)
    static member inline (+) (D0, D6) = (D0, D6)
    static member inline (+) (D0, D7) = (D0, D7)
    static member inline (+) (D0, D8) = (D0, D8)
    static member inline (+) (D0, D9) = (D0, D9)

  type D1 with
    static member inline (!!) D1 = D8
    static member inline (+) (D1, D0) = (D0, D1)
    static member inline (+) (D1, D1) = (D0, D2)
    static member inline (+) (D1, D2) = (D0, D3)
    static member inline (+) (D1, D3) = (D0, D4)
    static member inline (+) (D1, D4) = (D0, D5)
    static member inline (+) (D1, D5) = (D0, D6)
    static member inline (+) (D1, D6) = (D0, D7)
    static member inline (+) (D1, D7) = (D0, D8)
    static member inline (+) (D1, D8) = (D0, D9)
    static member inline (+) (D1, D9) = (D1, D0)

  type D2 with
    static member inline (!!) D2 = D7
    static member inline (+) (D2, D0) = (D0, D2)
    static member inline (+) (D2, D1) = (D0, D3)
    static member inline (+) (D2, D2) = (D0, D4)
    static member inline (+) (D2, D3) = (D0, D5)
    static member inline (+) (D2, D4) = (D0, D6)
    static member inline (+) (D2, D5) = (D0, D7)
    static member inline (+) (D2, D6) = (D0, D8)
    static member inline (+) (D2, D7) = (D0, D9)
    static member inline (+) (D2, D8) = (D1, D0)
    static member inline (+) (D2, D9) = (D1, D1)

  type D3 with
    static member inline (!!) D3 = D6
    static member inline (+) (D3, D0) = (D0, D3)
    static member inline (+) (D3, D1) = (D0, D4)
    static member inline (+) (D3, D2) = (D0, D5)
    static member inline (+) (D3, D3) = (D0, D6)
    static member inline (+) (D3, D4) = (D0, D7)
    static member inline (+) (D3, D5) = (D0, D8)
    static member inline (+) (D3, D6) = (D0, D9)
    static member inline (+) (D3, D7) = (D1, D0)
    static member inline (+) (D3, D8) = (D1, D1)
    static member inline (+) (D3, D9) = (D1, D2)

  type D4 with
    static member inline (!!) D4 = D5
    static member inline (+) (D4, D0) = (D0, D4)
    static member inline (+) (D4, D1) = (D0, D5)
    static member inline (+) (D4, D2) = (D0, D6)
    static member inline (+) (D4, D3) = (D0, D7)
    static member inline (+) (D4, D4) = (D0, D8)
    static member inline (+) (D4, D5) = (D0, D9)
    static member inline (+) (D4, D6) = (D1, D0)
    static member inline (+) (D4, D7) = (D1, D1)
    static member inline (+) (D4, D8) = (D1, D2)
    static member inline (+) (D4, D9) = (D1, D3)

  type D5 with
    static member inline (!!) D5 = D4
    static member inline (+) (D5, D0) = (D0, D5)
    static member inline (+) (D5, D1) = (D0, D6)
    static member inline (+) (D5, D2) = (D0, D7)
    static member inline (+) (D5, D3) = (D0, D8)
    static member inline (+) (D5, D4) = (D0, D9)
    static member inline (+) (D5, D5) = (D1, D0)
    static member inline (+) (D5, D6) = (D1, D1)
    static member inline (+) (D5, D7) = (D1, D2)
    static member inline (+) (D5, D8) = (D1, D3)
    static member inline (+) (D5, D9) = (D1, D4)

  type D6 with
    static member inline (!!) D6 = D3
    static member inline (+) (D6, D0) = (D0, D6)
    static member inline (+) (D6, D1) = (D0, D7)
    static member inline (+) (D6, D2) = (D0, D8)
    static member inline (+) (D6, D3) = (D0, D9)
    static member inline (+) (D6, D4) = (D1, D0)
    static member inline (+) (D6, D5) = (D1, D1)
    static member inline (+) (D6, D6) = (D1, D2)
    static member inline (+) (D6, D7) = (D1, D3)
    static member inline (+) (D6, D8) = (D1, D4)
    static member inline (+) (D6, D9) = (D1, D5)

  type D7 with
    static member inline (!!) D7 = D2
    static member inline (+) (D7, D0) = (D0, D7)
    static member inline (+) (D7, D1) = (D0, D8)
    static member inline (+) (D7, D2) = (D0, D9)
    static member inline (+) (D7, D3) = (D1, D0)
    static member inline (+) (D7, D4) = (D1, D1)
    static member inline (+) (D7, D5) = (D1, D2)
    static member inline (+) (D7, D6) = (D1, D3)
    static member inline (+) (D7, D7) = (D1, D4)
    static member inline (+) (D7, D8) = (D1, D5)
    static member inline (+) (D7, D9) = (D1, D6)

  type D8 with
    static member inline (!!) D8 = D1
    static member inline (+) (D8, D0) = (D0, D8)
    static member inline (+) (D8, D1) = (D0, D9)
    static member inline (+) (D8, D2) = (D1, D0)
    static member inline (+) (D8, D3) = (D1, D1)
    static member inline (+) (D8, D4) = (D1, D2)
    static member inline (+) (D8, D5) = (D1, D3)
    static member inline (+) (D8, D6) = (D1, D4)
    static member inline (+) (D8, D7) = (D1, D5)
    static member inline (+) (D8, D8) = (D1, D6)
    static member inline (+) (D8, D9) = (D1, D7)

  type D9 with
    static member inline (!!) D9 = D0
    static member inline (+) (D9, D0) = (D0, D9)
    static member inline (+) (D9, D1) = (D1, D0)
    static member inline (+) (D9, D2) = (D1, D1)
    static member inline (+) (D9, D3) = (D1, D2)
    static member inline (+) (D9, D4) = (D1, D3)
    static member inline (+) (D9, D5) = (D1, D4)
    static member inline (+) (D9, D6) = (D1, D5)
    static member inline (+) (D9, D7) = (D1, D6)
    static member inline (+) (D9, D8) = (D1, D7)
    static member inline (+) (D9, D9) = (D1, D8)

  type D0 with static member inline (.*) (D0, nn : ^nn) = ( ^nn : (static member Zero : ^zero) ())
  type D1 with static member inline (.*) (D1, nn) = nn
  type D2 with static member inline (.*) (D2, nn) = nn + nn
  type D3 with static member inline (.*) (D3, nn) = nn + nn + nn
  type D4 with static member inline (.*) (D4, nn) = nn + nn + nn + nn
  type D5 with static member inline (.*) (D5, nn) = nn + nn + nn + nn + nn
  type D6 with static member inline (.*) (D6, nn) = nn + nn + nn + nn + nn + nn
  type D7 with static member inline (.*) (D7, nn) = nn + nn + nn + nn + nn + nn + nn
  type D8 with static member inline (.*) (D8, nn) = nn + nn + nn + nn + nn + nn + nn + nn
  type D9 with static member inline (.*) (D9, nn) = nn + nn + nn + nn + nn + nn + nn + nn + nn

