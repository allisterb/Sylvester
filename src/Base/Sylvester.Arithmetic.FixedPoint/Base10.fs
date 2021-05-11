#nowarn "0077"
namespace Sylvester

open System 

[<NoEquality; NoComparison>]
type Base10Digit = 
  abstract member Val:int
  abstract member Val_:uint64

[<AutoOpen>]
module Base10 =
    
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
    
  type ``0``() = interface I0 with member x.Val = 0 member x.Val_ = 0UL
  type ``1``() = interface I1 with member x.Val = 1 member x.Val_ = 1UL
  type ``2``() = interface I2 with member x.Val = 2 member x.Val_ = 2UL
  type ``3``() = interface I3 with member x.Val = 3 member x.Val_ = 3UL
  type ``4``() = interface I4 with member x.Val = 4 member x.Val_ = 4UL
  type ``5``() = interface I5 with member x.Val = 5 member x.Val_ = 5UL
  type ``6``() = interface I6 with member x.Val = 6 member x.Val_ = 6UL
  type ``7``() = interface I7 with member x.Val = 7 member x.Val_ = 7UL
  type ``8``() = interface I8 with member x.Val = 8 member x.Val_ = 8UL
  type ``9``() = interface I9 with member x.Val = 9 member x.Val_ = 9UL

  type _0 = ``0``
  type _1 = ``1``
  type _2 = ``2``
  type _3 = ``3``
  type _4 = ``4``
  type _5 = ``5``
  type _6 = ``6``
  type _7 = ``7``
  type _8 = ``8``
  type _9 = ``9``

  let d0 = _0()
  let d1 = _1()
  let d2 = _2()
  let d3 = _3()
  let d4 = _4()
  let d5 = _5()
  let d6 = _6()
  let d7 = _7()
  let d8 = _8()
  let d9 = _9()

  type ``0`` with
    static member Zero = d0
    static member inline (!!) (_:_0) = d9
    static member inline (+) (_:_0, _:_0) = (d0, d0)
    static member inline (+) (_:_0, _:_1) = (d0, d1)
    static member inline (+) (_:_0, _:_2) = (d0, d2)
    static member inline (+) (_:_0, _:_3) = (d0, d3)
    static member inline (+) (_:_0, _:_4) = (d0, d4)
    static member inline (+) (_:_0, _:_5) = (d0, d5)
    static member inline (+) (_:_0, _:_6) = (d0, d6)
    static member inline (+) (_:_0, _:_7) = (d0, d7)
    static member inline (+) (_:_0, _:_8) = (d0, d8)
    static member inline (+) (_:_0, _:_9) = (d0, d9)

    static member inline (+>>>) (_:_0, _:_0) = (d1, d0)
    static member inline (+>>>) (_:_0, _:_1) = (d1, d1)
    static member inline (+>>>) (_:_0, _:_2) = (d1, d1)
    static member inline (+>>>) (_:_0, _:_3) = (d1, d1)
    static member inline (+>>>) (_:_0, _:_4) = (d1, d1)
    static member inline (+>>>) (_:_0, _:_5) = (d1, d1)
    static member inline (+>>>) (_:_0, _:_6) = (d1, d1)
    static member inline (+>>>) (_:_0, _:_7) = (d1, d1)
    static member inline (+>>>) (_:_0, _:_8) = (d1, d1)
    static member inline (+>>>) (_:_0, _:_9) = (d1, d1)

    static member inline (+===) (_:_0, _:_0) = _true
    static member inline (+===) (_:_0, _:_1) = _false
    static member inline (+===) (_:_0, _:_2) = _false
    static member inline (+===) (_:_0, _:_3) = _false
    static member inline (+===) (_:_0, _:_4) = _false
    static member inline (+===) (_:_0, _:_5) = _false
    static member inline (+===) (_:_0, _:_6) = _false
    static member inline (+===) (_:_0, _:_7) = _false
    static member inline (+===) (_:_0, _:_8) = _false
    static member inline (+===) (_:_0, _:_9) = _false

    static member inline (!??) (_:_0) = _true
  
  type ``1`` with
    static member Zero = d0
    static member inline (!!) (_:_1) = d8
    static member inline (+) (_:_1, _:_0) = (d0, d1)
    static member inline (+) (_:_1, _:_1) = (d0, d2)
    static member inline (+) (_:_1, _:_2) = (d0, d3)
    static member inline (+) (_:_1, _:_3) = (d0, d4)
    static member inline (+) (_:_1, _:_4) = (d0, d5)
    static member inline (+) (_:_1, _:_5) = (d0, d6)
    static member inline (+) (_:_1, _:_6) = (d0, d7)
    static member inline (+) (_:_1, _:_7) = (d0, d8)
    static member inline (+) (_:_1, _:_8) = (d0, d9)
    static member inline (+) (_:_1, _:_9) = (d1, d0)

    static member inline (+>>>) (_:_1, _:_0) = (d0, d0)
    static member inline (+>>>) (_:_1, _:_1) = (d1, d0)
    static member inline (+>>>) (_:_1, _:_2) = (d1, d1)
    static member inline (+>>>) (_:_1, _:_3) = (d1, d1)
    static member inline (+>>>) (_:_1, _:_4) = (d1, d1)
    static member inline (+>>>) (_:_1, _:_5) = (d1, d1)
    static member inline (+>>>) (_:_1, _:_6) = (d1, d1)
    static member inline (+>>>) (_:_1, _:_7) = (d1, d1)
    static member inline (+>>>) (_:_1, _:_8) = (d1, d1)
    static member inline (+>>>) (_:_1, _:_9) = (d1, d1)

    static member inline (+===) (_:_1, _:_0) = _false
    static member inline (+===) (_:_1, _:_1) = _true
    static member inline (+===) (_:_1, _:_2) = _false
    static member inline (+===) (_:_1, _:_3) = _false
    static member inline (+===) (_:_1, _:_4) = _false
    static member inline (+===) (_:_1, _:_5) = _false
    static member inline (+===) (_:_1, _:_6) = _false
    static member inline (+===) (_:_1, _:_7) = _false
    static member inline (+===) (_:_1, _:_8) = _false
    static member inline (+===) (_:_1, _:_9) = _false

    static member inline (!??) (_:_1) = _false
  
  type ``2`` with
    static member Zero = d0
    static member inline (!!) (_:_2) = d7
    static member inline (+) (_:_2, _:_0) = (d0, d2)
    static member inline (+) (_:_2, _:_1) = (d0, d3)
    static member inline (+) (_:_2, _:_2) = (d0, d4)
    static member inline (+) (_:_2, _:_3) = (d0, d5)
    static member inline (+) (_:_2, _:_4) = (d0, d6)
    static member inline (+) (_:_2, _:_5) = (d0, d7)
    static member inline (+) (_:_2, _:_6) = (d0, d8)
    static member inline (+) (_:_2, _:_7) = (d0, d9)
    static member inline (+) (_:_2, _:_8) = (d1, d0)
    static member inline (+) (_:_2, _:_9) = (d1, d1)

    static member inline (+>>>) (_:_2, _:_0) = (d0, d0)
    static member inline (+>>>) (_:_2, _:_1) = (d0, d0)
    static member inline (+>>>) (_:_2, _:_2) = (d1, d0)
    static member inline (+>>>) (_:_2, _:_3) = (d1, d1)
    static member inline (+>>>) (_:_2, _:_4) = (d1, d1)
    static member inline (+>>>) (_:_2, _:_5) = (d1, d1)
    static member inline (+>>>) (_:_2, _:_6) = (d1, d1)
    static member inline (+>>>) (_:_2, _:_7) = (d1, d1)
    static member inline (+>>>) (_:_2, _:_8) = (d1, d1)
    static member inline (+>>>) (_:_2, _:_9) = (d1, d1)

    static member inline (+===) (_:_2, _:_0) = _false
    static member inline (+===) (_:_2, _:_1) = _false
    static member inline (+===) (_:_2, _:_2) = _true
    static member inline (+===) (_:_2, _:_3) = _false
    static member inline (+===) (_:_2, _:_4) = _false
    static member inline (+===) (_:_2, _:_5) = _false
    static member inline (+===) (_:_2, _:_6) = _false
    static member inline (+===) (_:_2, _:_7) = _false
    static member inline (+===) (_:_2, _:_8) = _false
    static member inline (+===) (_:_2, _:_9) = _false

    static member inline (!??) (_:_2) = _false

  type ``3`` with
    static member Zero = d0
    static member inline (!!) (_:_3) = d6
    static member inline (+) (_:_3, _:_0) = (d0, d3)
    static member inline (+) (_:_3, _:_1) = (d0, d4)
    static member inline (+) (_:_3, _:_2) = (d0, d5)
    static member inline (+) (_:_3, _:_3) = (d0, d6)
    static member inline (+) (_:_3, _:_4) = (d0, d7)
    static member inline (+) (_:_3, _:_5) = (d0, d8)
    static member inline (+) (_:_3, _:_6) = (d0, d9)
    static member inline (+) (_:_3, _:_7) = (d1, d0)
    static member inline (+) (_:_3, _:_8) = (d1, d1)
    static member inline (+) (_:_3, _:_9) = (d1, d2)

    static member inline (+>>>) (_:_3, _:_0) = (d0, d0)
    static member inline (+>>>) (_:_3, _:_1) = (d0, d0)
    static member inline (+>>>) (_:_3, _:_2) = (d0, d0)
    static member inline (+>>>) (_:_3, _:_3) = (d1, d0)
    static member inline (+>>>) (_:_3, _:_4) = (d1, d1)
    static member inline (+>>>) (_:_3, _:_5) = (d1, d1)
    static member inline (+>>>) (_:_3, _:_6) = (d1, d1)
    static member inline (+>>>) (_:_3, _:_7) = (d1, d1)
    static member inline (+>>>) (_:_3, _:_8) = (d1, d1)
    static member inline (+>>>) (_:_3, _:_9) = (d1, d1)

    static member inline (+===) (_:_3, _:_0) = _false
    static member inline (+===) (_:_3, _:_1) = _false
    static member inline (+===) (_:_3, _:_2) = _false
    static member inline (+===) (_:_3, _:_3) = _true
    static member inline (+===) (_:_3, _:_4) = _false
    static member inline (+===) (_:_3, _:_5) = _false
    static member inline (+===) (_:_3, _:_6) = _false
    static member inline (+===) (_:_3, _:_7) = _false
    static member inline (+===) (_:_3, _:_8) = _false
    static member inline (+===) (_:_3, _:_9) = _false

    static member inline (!??) (_:_3) = _false

  type ``4`` with
    static member Zero = d0
    static member inline (!!) (_:_4) = d5
    static member inline (+) (_:_4, _:_0) = (d0, d4)
    static member inline (+) (_:_4, _:_1) = (d0, d5)
    static member inline (+) (_:_4, _:_2) = (d0, d6)
    static member inline (+) (_:_4, _:_3) = (d0, d7)
    static member inline (+) (_:_4, _:_4) = (d0, d8)
    static member inline (+) (_:_4, _:_5) = (d0, d9)
    static member inline (+) (_:_4, _:_6) = (d1, d0)
    static member inline (+) (_:_4, _:_7) = (d1, d1)
    static member inline (+) (_:_4, _:_8) = (d1, d2)
    static member inline (+) (_:_4, _:_9) = (d1, d3)

    static member inline (+>>>) (_:_4, _:_0) = (d0, d0)
    static member inline (+>>>) (_:_4, _:_1) = (d0, d0)
    static member inline (+>>>) (_:_4, _:_2) = (d0, d0)
    static member inline (+>>>) (_:_4, _:_3) = (d0, d0)
    static member inline (+>>>) (_:_4, _:_4) = (d1, d0)
    static member inline (+>>>) (_:_4, _:_5) = (d1, d1)
    static member inline (+>>>) (_:_4, _:_6) = (d1, d1)
    static member inline (+>>>) (_:_4, _:_7) = (d1, d1)
    static member inline (+>>>) (_:_4, _:_8) = (d1, d1)
    static member inline (+>>>) (_:_4, _:_9) = (d1, d1)

    static member inline (+===) (_:_4, _:_0) = _false
    static member inline (+===) (_:_4, _:_1) = _false
    static member inline (+===) (_:_4, _:_2) = _false
    static member inline (+===) (_:_4, _:_3) = _false
    static member inline (+===) (_:_4, _:_4) = _true
    static member inline (+===) (_:_4, _:_5) = _false
    static member inline (+===) (_:_4, _:_6) = _false
    static member inline (+===) (_:_4, _:_7) = _false
    static member inline (+===) (_:_4, _:_8) = _false
    static member inline (+===) (_:_4, _:_9) = _false

    static member inline (!??) (_:_4) = _false

  type ``5`` with
    static member Zero = d0
    static member inline (!!) (_:_5) = d4
    static member inline (+) (_:_5, _:_0) = (d0, d5)
    static member inline (+) (_:_5, _:_1) = (d0, d6)
    static member inline (+) (_:_5, _:_2) = (d0, d7)
    static member inline (+) (_:_5, _:_3) = (d0, d8)
    static member inline (+) (_:_5, _:_4) = (d0, d9)
    static member inline (+) (_:_5, _:_5) = (d1, d0)
    static member inline (+) (_:_5, _:_6) = (d1, d1)
    static member inline (+) (_:_5, _:_7) = (d1, d2)
    static member inline (+) (_:_5, _:_8) = (d1, d3)
    static member inline (+) (_:_5, _:_9) = (d1, d4)

    static member inline (+>>>) (_:_5, _:_0) = (d0, d0)
    static member inline (+>>>) (_:_5, _:_1) = (d0, d0)
    static member inline (+>>>) (_:_5, _:_2) = (d0, d0)
    static member inline (+>>>) (_:_5, _:_3) = (d0, d0)
    static member inline (+>>>) (_:_5, _:_4) = (d0, d0)
    static member inline (+>>>) (_:_5, _:_5) = (d1, d0)
    static member inline (+>>>) (_:_5, _:_6) = (d1, d1)
    static member inline (+>>>) (_:_5, _:_7) = (d1, d1)
    static member inline (+>>>) (_:_5, _:_8) = (d0, d1)
    static member inline (+>>>) (_:_5, _:_9) = (d0, d1)

    static member inline (+===) (_:_5, _:_0) = _false
    static member inline (+===) (_:_5, _:_1) = _false
    static member inline (+===) (_:_5, _:_2) = _false
    static member inline (+===) (_:_5, _:_3) = _false
    static member inline (+===) (_:_5, _:_4) = _false
    static member inline (+===) (_:_5, _:_5) = _true
    static member inline (+===) (_:_5, _:_6) = _false
    static member inline (+===) (_:_5, _:_7) = _false
    static member inline (+===) (_:_5, _:_8) = _false
    static member inline (+===) (_:_5, _:_9) = _false

    static member inline (!??) (_:_5) = _false

  type ``6`` with
    static member Zero = d0
    static member inline (!!) (_:_6) = d3
    static member inline (+) (_:_6, _:_0) = (d0, d6)
    static member inline (+) (_:_6, _:_1) = (d0, d7)
    static member inline (+) (_:_6, _:_2) = (d0, d8)
    static member inline (+) (_:_6, _:_3) = (d0, d9)
    static member inline (+) (_:_6, _:_4) = (d1, d0)
    static member inline (+) (_:_6, _:_5) = (d1, d1)
    static member inline (+) (_:_6, _:_6) = (d1, d2)
    static member inline (+) (_:_6, _:_7) = (d1, d3)
    static member inline (+) (_:_6, _:_8) = (d1, d4)
    static member inline (+) (_:_6, _:_9) = (d1, d5)

    static member inline (+>>>) (_:_6, _:_0) = (d0, d0)
    static member inline (+>>>) (_:_6, _:_1) = (d0, d0)
    static member inline (+>>>) (_:_6, _:_2) = (d1, d0)
    static member inline (+>>>) (_:_6, _:_3) = (d0, d0)
    static member inline (+>>>) (_:_6, _:_4) = (d0, d0)
    static member inline (+>>>) (_:_6, _:_5) = (d0, d0)
    static member inline (+>>>) (_:_6, _:_6) = (d1, d0)
    static member inline (+>>>) (_:_6, _:_7) = (d1, d1)
    static member inline (+>>>) (_:_6, _:_8) = (d1, d1)
    static member inline (+>>>) (_:_6, _:_9) = (d1, d1)

    static member inline (+===) (_:_6, _:_0) = _false
    static member inline (+===) (_:_6, _:_1) = _false
    static member inline (+===) (_:_6, _:_2) = _false
    static member inline (+===) (_:_6, _:_3) = _false
    static member inline (+===) (_:_6, _:_4) = _false
    static member inline (+===) (_:_6, _:_5) = _false
    static member inline (+===) (_:_6, _:_6) = _true
    static member inline (+===) (_:_6, _:_7) = _false
    static member inline (+===) (_:_6, _:_8) = _false
    static member inline (+===) (_:_6, _:_9) = _false

    static member inline (!??) (_:_6) = _false

  type ``7`` with
    static member Zero = d0
    static member inline (!!) (_:_7) = d2
    static member inline (+) (_:_7, _:_0) = (d0, d7)
    static member inline (+) (_:_7, _:_1) = (d0, d8)
    static member inline (+) (_:_7, _:_2) = (d0, d9)
    static member inline (+) (_:_7, _:_3) = (d1, d0)
    static member inline (+) (_:_7, _:_4) = (d1, d1)
    static member inline (+) (_:_7, _:_5) = (d1, d2)
    static member inline (+) (_:_7, _:_6) = (d1, d3)
    static member inline (+) (_:_7, _:_7) = (d1, d4)
    static member inline (+) (_:_7, _:_8) = (d1, d5)
    static member inline (+) (_:_7, _:_9) = (d1, d6)

    static member inline (+>>>) (_:_7, _:_0) = (d0, d0)
    static member inline (+>>>) (_:_7, _:_1) = (d0, d0)
    static member inline (+>>>) (_:_7, _:_2) = (d0, d0)
    static member inline (+>>>) (_:_7, _:_3) = (d0, d0)
    static member inline (+>>>) (_:_7, _:_4) = (d0, d0)
    static member inline (+>>>) (_:_7, _:_5) = (d0, d0)
    static member inline (+>>>) (_:_7, _:_6) = (d0, d0)
    static member inline (+>>>) (_:_7, _:_7) = (d1, d0)
    static member inline (+>>>) (_:_7, _:_8) = (d1, d1)
    static member inline (+>>>) (_:_7, _:_9) = (d1, d1)

    static member inline (+===) (_:_7, _:_0) = _false
    static member inline (+===) (_:_7, _:_1) = _false
    static member inline (+===) (_:_7, _:_2) = _false
    static member inline (+===) (_:_7, _:_3) = _false
    static member inline (+===) (_:_7, _:_4) = _false
    static member inline (+===) (_:_7, _:_5) = _false
    static member inline (+===) (_:_7, _:_6) = _false
    static member inline (+===) (_:_7, _:_7) = _true
    static member inline (+===) (_:_7, _:_8) = _false
    static member inline (+===) (_:_7, _:_9) = _false

    static member inline (!??) (_:_7) = _false

  type ``8`` with
    static member Zero = d0
    static member inline (!!) (_:_8) = d1
    static member inline (+) (_:_8, _:_0) = (d0, d8)
    static member inline (+) (_:_8, _:_1) = (d0, d9)
    static member inline (+) (_:_8, _:_2) = (d1, d0)
    static member inline (+) (_:_8, _:_3) = (d1, d1)
    static member inline (+) (_:_8, _:_4) = (d1, d2)
    static member inline (+) (_:_8, _:_5) = (d1, d3)
    static member inline (+) (_:_8, _:_6) = (d1, d4)
    static member inline (+) (_:_8, _:_7) = (d1, d5)
    static member inline (+) (_:_8, _:_8) = (d1, d6)
    static member inline (+) (_:_8, _:_9) = (d1, d7)

    static member inline (+>>>) (_:_8, _:_0) = (d0, d0)
    static member inline (+>>>) (_:_8, _:_1) = (d0, d0)
    static member inline (+>>>) (_:_8, _:_2) = (d1, d0)
    static member inline (+>>>) (_:_8, _:_3) = (d0, d0)
    static member inline (+>>>) (_:_8, _:_4) = (d0, d0)
    static member inline (+>>>) (_:_8, _:_5) = (d0, d0)
    static member inline (+>>>) (_:_8, _:_6) = (d0, d0)
    static member inline (+>>>) (_:_8, _:_7) = (d0, d0)
    static member inline (+>>>) (_:_8, _:_8) = (d1, d0)
    static member inline (+>>>) (_:_8, _:_9) = (d1, d1)

    static member inline (+===) (_:_8, _:_0) = _false
    static member inline (+===) (_:_8, _:_1) = _false
    static member inline (+===) (_:_8, _:_2) = _false
    static member inline (+===) (_:_8, _:_3) = _false
    static member inline (+===) (_:_8, _:_4) = _false
    static member inline (+===) (_:_8, _:_5) = _false
    static member inline (+===) (_:_8, _:_6) = _false
    static member inline (+===) (_:_8, _:_7) = _false
    static member inline (+===) (_:_8, _:_8) = _true
    static member inline (+===) (_:_8, _:_9) = _false

    static member inline (!??) (_:_8) = _false

  type ``9`` with
    static member Zero = d0
    static member inline (!!) (_:_9) = d0
    static member inline (+) (_:_9, _:_0) = (d0, d9)
    static member inline (+) (_:_9, _:_1) = (d1, d0)
    static member inline (+) (_:_9, _:_2) = (d1, d1)
    static member inline (+) (_:_9, _:_3) = (d1, d2)
    static member inline (+) (_:_9, _:_4) = (d1, d3)
    static member inline (+) (_:_9, _:_5) = (d1, d4)
    static member inline (+) (_:_9, _:_6) = (d1, d5)
    static member inline (+) (_:_9, _:_7) = (d1, d6)
    static member inline (+) (_:_9, _:_8) = (d1, d7)
    static member inline (+) (_:_9, _:_9) = (d1, d8)

    static member inline (+>>>) (_:_9, _:_0) = (d0, d0)
    static member inline (+>>>) (_:_9, _:_1) = (d0, d0)
    static member inline (+>>>) (_:_9, _:_2) = (d0, d0)
    static member inline (+>>>) (_:_9, _:_3) = (d0, d0)
    static member inline (+>>>) (_:_9, _:_4) = (d0, d0)
    static member inline (+>>>) (_:_9, _:_5) = (d0, d0)
    static member inline (+>>>) (_:_9, _:_6) = (d0, d0)
    static member inline (+>>>) (_:_9, _:_7) = (d0, d0)
    static member inline (+>>>) (_:_9, _:_8) = (d0, d0)
    static member inline (+>>>) (_:_9, _:_9) = (d1, d0)

    static member inline (+===) (_:_9, _:_0) = _false
    static member inline (+===) (_:_9, _:_1) = _false
    static member inline (+===) (_:_9, _:_2) = _false
    static member inline (+===) (_:_9, _:_3) = _false
    static member inline (+===) (_:_9, _:_4) = _false
    static member inline (+===) (_:_9, _:_5) = _false
    static member inline (+===) (_:_9, _:_6) = _false
    static member inline (+===) (_:_9, _:_7) = _false
    static member inline (+===) (_:_9, _:_8) = _false
    static member inline (+===) (_:_9, _:_9) = _true

    static member inline (!??) (_:_9) = _false

  type ``0`` with static member inline (++*) (_:_0, nn : ^nn) = ( ^nn : (static member Zero : ^zero) ())
  type ``1`` with static member inline (++*) (_:_1, nn) = nn
  type ``2`` with static member inline (++*) (_:_2, nn) = nn + nn
  type ``3`` with static member inline (++*) (_:_3, nn) = nn + nn + nn
  type ``4`` with static member inline (++*) (_:_4, nn) = nn + nn + nn + nn
  type ``5`` with static member inline (++*) (_:_5, nn) = nn + nn + nn + nn + nn
  type ``6`` with static member inline (++*) (_:_6, nn) = nn + nn + nn + nn + nn + nn
  type ``7`` with static member inline (++*) (_:_7, nn) = nn + nn + nn + nn + nn + nn + nn
  type ``8`` with static member inline (++*) (_:_8, nn) = nn + nn + nn + nn + nn + nn + nn + nn
  type ``9`` with static member inline (++*) (_:_9, nn) = nn + nn + nn + nn + nn + nn + nn + nn + nn

  let digit<'d when 'd :>Base10Digit>() :'d = Activator.CreateInstance<'d>()
   
  let _val(d:Base10Digit) = (uint64) d.Val

  let isEqualTo<'d when 'd :> Base10Digit>(b: obj) =
    match b with 
        | :? 'd ->  true            
        | _  -> false

  let areEqual<'d when 'd :> Base10Digit>(_:'d, b: obj) = isEqualTo<'d>(b)

  let getCharBase10Type(d:char) =
    match d with
    | '0' -> typedefof<_0>
    | '1' -> typedefof<_1>
    | '2' -> typedefof<_2>
    | '3' -> typedefof<_3>
    | '4' -> typedefof<_4>
    | '5' -> typedefof<_5>
    | '6' -> typedefof<_6>
    | '7' -> typedefof<_7>
    | '8' -> typedefof<_8>
    | '9' -> typedefof<_9>
    | _ -> failwith "Invalid char digit."

  let getIntBase10TypeArray(n:int, length:int) =
    let s = n.ToString()
    do if s.Length > length then failwith <| sprintf "The number of integer digits in %i is more than %i." n length
    s.PadLeft(length, '0') |> Seq.map(fun d -> getCharBase10Type d) |> Seq.toArray

  
  

  