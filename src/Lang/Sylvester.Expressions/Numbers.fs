﻿#nowarn "0077"

namespace Sylvester

open System
open System.Numerics
open FSharp.Quotations
open FSharp.Quotations.DerivedPatterns

[<CustomEquality; CustomComparison>]
type Rational = // Inspired by: https://github.com/mathnet/mathnet-numerics/blob/master/src/FSharp/BigRational.fs
    struct 
        val Numerator: BigInteger
        val Denominator: BigInteger
        new(p:BigInteger, q:BigInteger) = {Numerator = p; Denominator = q}
        new(p:BigInteger) = {Numerator = p; Denominator = BigInteger.One}
        new(p:Natural) = {Numerator = p.IntVal; Denominator = BigInteger.One}
        new(p:int, q:int) = {Numerator = BigInteger p; Denominator = BigInteger q}
        new(p:int64, q:int64) = {Numerator = BigInteger p; Denominator = BigInteger q}
        new(p:float, q:float) = {Numerator = BigInteger p; Denominator = BigInteger q}
        new(p:float32, q:float32) = {Numerator = BigInteger p; Denominator = BigInteger q}
        new(x: float) =
            let n,d = x.ToRational().ToTuple() in {Numerator= BigInteger n; Denominator = BigInteger d}
            // From: http://www.fssnip.net/kV/title/Convert-a-Float-to-a-Mixed-Number
            (*
            let wholePart = int x     // whole part of x
            let decimalPt = x % 1.0   // decimal part of x
            let rec cF(Z : float, i : int, Dm : float, Do : float) =
                match Z % 1.0 > Double.Epsilon, i < 1 with
                //  First case terminates after 14 iterations
                | _    , true  -> (wholePart, (int64 (System.Math.Round(decimalPt * Do)), int64 Do))
                //  Second case executes next cF (continuing fraction)
                | true , false -> cF(1.0/(Z % 1.0), i - 1 , Do, Do * System.Math.Round(1.0/(Z % 1.0)-0.5) + Dm )
                //  Final case terminates if the remainder of Z > 10^-6
                | false, _  -> (wholePart, (int64 (System.Math.Round(decimalPt * Do)), int64 Do))
            let w, (n, d) = decimalPt |> fun x -> cF(x, 14, 0.0, 1.0)
            let n' = ((w |> int64) * d) + n
            { Numerator = n' |> BigInteger; Denominator = d |> BigInteger }
            (Num
    *)
    end 
    member x.Tuple: ValueTuple<BigInteger, BigInteger> = x.Numerator, x.Denominator
    member x.Equals(y: Rational) = x.Tuple.Equals y.Tuple
    override x.Equals (y:obj) = 
        match y with 
        | :? Rational as r -> x.Equals r 
        | :? BigInteger as i -> Rational(i, BigInteger.One) |> x.Equals
        | :? Natural as n -> Rational(n) |> x.Equals
        | :? int as i -> Rational(i, 1) |> x.Equals
        | :? int64 as i -> Rational(i, 1L) |> x.Equals
        | :? float as f -> Rational(f, 1.) |> x.Equals
        | :? float32 as f -> Rational(f, 1.0f) |> x.Equals
        | _ -> false
    override x.GetHashCode () = if x.Denominator = BigInteger.One then x.Numerator.GetHashCode() else x.Tuple.GetHashCode()
    override x.ToString () = if x.Denominator = BigInteger.One then x.Numerator.ToString () else sprintf "%A/%A" x.Numerator x.Denominator
    interface IEquatable<Rational> with member a.Equals b = a.Tuple = b.Tuple
    interface IComparable<Rational> with
        member x.CompareTo y = (x.Numerator * y.Denominator).CompareTo(y.Numerator * x.Denominator)
    interface IComparable with
        member x.CompareTo y = 
            match y with 
            | :? Rational as r -> (x :> IComparable<Rational>).CompareTo r
            | :? BigInteger as i -> (x :> IComparable<Rational>).CompareTo (Rational i)
            | :? Natural as n -> (x :> IComparable<Rational>).CompareTo (Rational n)
            | :? int as i -> (x :> IComparable<Rational>).CompareTo (Rational(i, 1))
            | :? int64 as i -> (x :> IComparable<Rational>).CompareTo (Rational(i, 1L))
            | :? float as f -> (x :> IComparable<Rational>).CompareTo (Rational(f, 1.))
            | :? float32 as f -> (x :> IComparable<Rational>).CompareTo (Rational(f, 1.0f))
            | _ -> failwithf "The object %A does not have a comparable type." y
    interface IFormattable with 
        member x.ToString(f, p) = 
            if x.Denominator = BigInteger.One then x.Numerator.ToString(f, p) else sprintf "%s\u2044%s" (x.Numerator.ToString(f, p)) (x.Denominator.ToString(f, p))
    
    static member Zero = Rational(BigInteger.Zero)
    
    static member One = Rational(BigInteger.One)
    
    static member Parse (s : string) =
        let len = if s.Length = 0 then failwith "This string is empty." else s.Length
        let j = s.IndexOf '/'
        if j >= 0 then
            let p = BigInteger.Parse (s.Substring (0, j))
            let q = BigInteger.Parse (s.Substring (j + 1, len - j - 1))
            Rational(p, q)
        else
            let p = BigInteger.Parse s
            Rational(p, BigInteger.One)

    static member Normalize (p : BigInteger, q : BigInteger) =
        if q.IsZero then
            raise <| System.DivideByZeroException ()
        elif q.IsOne then
            Rational(p, q)
        else
            let k = BigInteger.GreatestCommonDivisor (p, q)
            let p = p / k
            let q = q / k
            if sign q < 0 then
                Rational(-p, -q)
            else
                Rational(p, q)

    static member Reciprocal (num : Rational) = Rational.Normalize (num.Denominator, num.Numerator)

    static member Pow (num : Rational, n : int) =
        if n < 0 then 
            Rational.Normalize (BigInteger.Pow (num.Denominator, -n), BigInteger.Pow (num.Numerator, -n))
        else 
            Rational (BigInteger.Pow (num.Numerator, n), BigInteger.Pow (num.Denominator, n))
        
    static member Pow (num : Rational, pow : Rational) =
        let g, p = (float) num,  (float) pow
        Rational(g ** p)

    static member Pow (num : Rational, pow : Natural) = Rational.Pow(num, Rational pow)

    static member Abs(r: Rational) = Rational((abs r.Numerator), (abs r.Denominator))

    static member Sqrt(r: Rational) = 
        let n, d = (float) r.Numerator, (float) r.Denominator
        Rational((sqrt n), (sqrt d))
    
    static member Sin(r:Rational) = (float) r |> sin |> Rational

    static member Cos(r:Rational) = (float) r |> cos |> Rational
    
    static member Tan(r:Rational) = (float) r |> tan |> Rational

    static member Sinh(r:Rational) = (float) r |> sinh |> Rational

    static member Cosh(r:Rational) = (float) r |> cosh |> Rational
    
    static member Tanh(r:Rational) = (float) r |> tanh |> Rational
    
    static member (~+) (r : Rational) = r

    static member (~-) (num : Rational) = Rational(-num.Numerator, num.Denominator)

    static member (+) (x : Rational, y : Rational) = Rational.Normalize ((x.Numerator * y.Denominator) + (y.Numerator * x.Denominator), x.Denominator * y.Denominator)

    static member (+) (x : Rational, y : int) = let y' = Rational (y, 1) in x + y'

    static member (+) (x : Rational, y : int64) = let y' = Rational (y, 1L) in x + y'
    
    static member (+) (x : Rational, y : BigInteger) = let y' = Rational (y, BigInteger.One) in x + y'

    static member (+) (x : Rational, y : float) = let y' = Rational (y, 1.) in x + y'

    static member (+) (x : Rational, y : float32) = let y' = Rational (y, 1.0f) in x + y'

    static member (+) (x : int, y : Rational) = let x' = Rational (x, 1) in x' + y
    
    static member (+) (x : int64, y : Rational) = let x' = Rational (x, 1L) in x' + y
    
    static member (+) (x : BigInteger, y : Rational) = let x' = Rational (x, BigInteger.One) in x' + y
    
    static member (+) (x : Natural, y : Rational) = let x' = Rational x in x' + y

    static member (+) (x : float, y : Rational) = let x' = Rational x in x' + y
    
    static member (+) (x : float32, y : Rational) = let x' = Rational ((float) x) in x' + y
    
    
    static member (-) (x : Rational, y : Rational) = Rational.Normalize ((x.Numerator * y.Denominator) - (y.Numerator * x.Denominator), x.Denominator * y.Denominator)
    
    static member (-) (x : Rational, y : int) = let y' = Rational (y, 1) in x - y'

    static member (-) (x : Rational, y : int64) = let y' = Rational (y, 1L) in x - y'
    
    static member (-) (x : Rational, y : BigInteger) = let y' = Rational (y, BigInteger.One) in x - y'

    static member (-) (x : Rational, y : Natural) = let y' = Rational y in x - y'

    static member (-) (x : Rational, y : float) = let y' = Rational y in x - y'

    static member (-) (x : Rational, y : float32) = let y' = Rational ((float) y) in x - y'

    static member (-) (x : int, y : Rational) = let x' = Rational (x, 1) in x' - y
    
    static member (-) (x : int64, y : Rational) = let x' = Rational (x, 1L) in x' - y
    
    static member (-) (x : BigInteger, y : Rational) = let x' = Rational (x, BigInteger.One) in x' - y
    
    static member (-) (x : Natural, y : Rational) = let x' = Rational x in x' - y

    static member (-) (x : float, y : Rational) = let x' = Rational x in x' - y
    
    static member (-) (x : float32, y : Rational) = let x' = Rational ((float) x) in x' - y
    
    
    static member (*) (x : Rational, y : Rational) = Rational.Normalize (x.Numerator * y.Numerator, x.Denominator * y.Denominator)
    
    static member (*) (x : Rational, y : int) = let y' = Rational (y, 1) in x * y'

    static member (*) (x : Rational, y : int64) = let y' = Rational (y, 1L) in x * y'
    
    static member (*) (x : Rational, y : BigInteger) = let y' = Rational (y, BigInteger.One) in x * y'

    static member (*) (x : Rational, y : Natural) = let y' = Rational y in x * y'

    static member (*) (x : Rational, y : float) = let y' = Rational y in x * y'

    static member (*) (x : Rational, y : float32) = let y' = Rational ((float) y) in x * y'

    static member (*) (x : int, y : Rational) = let x' = Rational (x, 1) in x' * y
    
    static member (*) (x : int64, y : Rational) = let x' = Rational (x, 1L) in x' * y
    
    static member (*) (x : BigInteger, y : Rational) = let x' = Rational (x, BigInteger.One) in x' * y
    
    static member (*) (x : Natural, y : Rational) = let x' = Rational x in x' * y

    static member (*) (x : float, y : Rational) = let x' = Rational x in x' * y
    
    static member (*) (x : float32, y : Rational) = let x' = Rational ((float) x) in x' * y
    
    
    static member (/) (x : Rational, y : Rational) = Rational.Normalize (x.Numerator * y.Denominator, x.Denominator * y.Numerator)
    
    static member (/) (x : Rational, y : int) = 
        if x.Denominator = BigInteger.One then 
            Rational(x.Numerator, BigInteger(y)) 
        else
            let y' = Rational (y, 1) in Rational.Normalize ((x.Numerator * y'.Denominator) / (y'.Numerator * x.Denominator), x.Denominator * y'.Denominator)

    static member (/) (x : Rational, y : int64) = let y' = Rational (y, 1L) in x / y'
    
    static member (/) (x : Rational, y : BigInteger) = let y' = Rational (y, BigInteger.One) in x / y'

    static member (/) (x : Rational, y : Natural) = let y' = Rational y in x / y'

    static member (/) (x : Rational, y : float) = let y' = Rational y in x / y'

    static member (/) (x : Rational, y : float32) = let y' = Rational ((float) y) in x / y'

    static member (/) (x : int, y : Rational) = let x' = Rational (x, 1) in x' / y
    
    static member (/) (x : int64, y : Rational) = let x' = Rational (x, 1L) in x' / y
    
    static member (/) (x : BigInteger, y : Rational) = let x' = Rational (x, BigInteger.One) in x' / y
    
    static member (/) (x : Natural, y : Rational) = let x' = Rational x in x' / y

    static member (/) (x : float, y : Rational) = let x' = Rational x in x' / y
    
    static member (/) (x : float32, y : Rational) = let x' = Rational ((float) x) in x' / y
    
    static member op_Explicit(r: Rational): float = (float) r.Numerator / (float) r.Denominator
    static member op_Explicit(r: Rational): float32 = (float32) r.Numerator / (float32) r.Denominator
    static member op_Explicit(r: Rational): decimal = (decimal) r.Numerator / (decimal) r.Denominator
    static member op_Explicit(r: Rational): BigInteger = 
        // have p = d.q + r, |r| < |q|
        let d, r = BigInteger.DivRem (r.Numerator, r.Denominator)
        if r < BigInteger.Zero then
            // p = (d-1).q + (r+q)
            d - BigInteger.One
        else
            // p = d.q + r
            d
    static member op_Explicit(r: Rational): MathNet.Numerics.BigRational = 
        MathNet.Numerics.BigRational.FromBigIntFraction(r.Numerator, r.Denominator)
    
    static member op_Explicit(r: BigInteger): Rational = Rational r
    static member op_Explicit(r: Natural): Rational = Rational r
    static member op_Explicit(r: int): Rational = Rational(r, 1)   
    static member op_Explicit(r: float): Rational = Rational(r)
    
    static member op_Equality (l:Rational, r:Rational) = l.Equals r
    static member op_Inequality (l:Rational, r:Rational) = not <| l.Equals r

and [<CustomEquality; CustomComparison>] Natural = 
    struct 
        val IntVal: BigInteger
        new(p:BigInteger) = {IntVal = if p >= BigInteger.Zero then p else failwithf "The value %A must be a positive integer." p}     
        new(p:int) = {IntVal = if p >= 0 then BigInteger p else failwithf "The value %A must be a positive integer." p} 
        new(p:int64) = {IntVal = if p >= 0L then BigInteger p else failwithf "The value %A must be a positive integer." p} 
        new(p:float) = {IntVal = if p >= 0. && p = floor p then BigInteger p else failwithf "The value %A must be a positive integer." p} 
        new(p:float32) = {IntVal = if p >= 0.0f && p = floor p then BigInteger p else failwithf "The value %A must be a positive integer." p}     
    end 
    
    member x.Equals(y: Natural) = x.IntVal.Equals y.IntVal
    override x.Equals (y:obj) = 
        match y with 
        | :? Rational as r -> x.IntVal.Equals r 
        | :? BigInteger as i -> Rational(i, BigInteger.One) |> x.IntVal.Equals
        | :? Natural as n -> x.IntVal.Equals n.IntVal
        | :? int as i -> Rational(i, 1) |> x.IntVal.Equals
        | :? int64 as i -> Rational(i, 1L) |> x.IntVal.Equals
        | :? float as f -> Rational(f, 1.) |> x.IntVal.Equals
        | :? float32 as f -> Rational(f, 1.0f) |> x.IntVal.Equals
        | _ -> false
    override x.GetHashCode () = x.IntVal.GetHashCode()
    override x.ToString () = sprintf "%AN" x.IntVal
    interface IEquatable<Natural> with member a.Equals b = a.IntVal = b.IntVal
    interface IComparable<Natural> with
        member x.CompareTo y = (x.IntVal).CompareTo(y.IntVal)
    interface IComparable with  
        member x.CompareTo y = 
            match y with 
            | :? Rational as r -> (x.IntVal).CompareTo r
            | :? BigInteger as i -> (x.IntVal).CompareTo(i)
            | :? Natural as n -> (x.IntVal).CompareTo(n.IntVal)
            | :? int as i -> (x.IntVal).CompareTo (Rational(i, 1))
            | :? int64 as i -> (x.IntVal).CompareTo (Rational(i, 1L))
            | :? float as f -> (x.IntVal).CompareTo (Rational(f, 1.))
            | :? float32 as f -> (x.IntVal).CompareTo (Rational(f, 1.0f))
            | _ -> failwithf "The object %A does not have a comparable type." y
    interface IFormattable with 
        member x.ToString(f, p) = x.IntVal.ToString(f, p) 
    
    static member Zero = Natural(BigInteger.Zero)
    
    static member One = Natural(BigInteger.One)
    
    static member Reciprocal (num : Natural) = Rational.Normalize (Natural.One.IntVal, num.IntVal)

    static member Pow (num : Natural, n : int) =
        if n < 0 then
            Rational.Normalize(BigInteger.One, BigInteger.Pow (num.IntVal, -n)) 
        else
            BigInteger.Pow (num.IntVal, n) |> Rational

    static member Pow (num : Natural, pow : Natural) =
       Natural.Pow(num, (int) pow)
       
    static member Abs(r: Natural) = r

    static member Sqrt(r: Natural) = 
        r.IntVal |> float |> sqrt
    
    static member (~+) (r : Natural) = r

    static member (~-) (num : Natural) = Rational(-num.IntVal, BigInteger.One)

    static member (+) (x : Natural, y : Natural) = Natural(x.IntVal + y.IntVal)

    static member (+) (x : Natural, y : int) = x + Natural y

    static member (+) (x : Natural, y : int64) = x + Natural y
    
    static member (+) (x : Natural, y : BigInteger) = x + Natural y

    static member (+) (x : Natural, y : float) = x + Natural y

    static member (+) (x : Natural, y : float32) = x + Natural y

    static member (+) (x : int, y : Natural) = Natural x + y
    
    static member (+) (x : int64, y : Natural) = Natural x + y
    
    static member (+) (x : BigInteger, y : Natural) = Natural x + y
    
    static member (+) (x : float, y : Natural) = Natural x + y
    
    static member (+) (x : float32, y : Natural) = Natural x + y
    

    static member (-) (x : Natural, y : Natural) = Natural(x.IntVal - y.IntVal)

    static member (-) (x : Natural, y : int) = x - Natural y

    static member (-) (x : Natural, y : int64) = x - Natural y

    static member (-) (x : Natural, y : BigInteger) = x - Natural y

    static member (-) (x : Natural, y : float) = x - Natural y

    static member (-) (x : Natural, y : float32) = x - Natural y

    static member (-) (x : int, y : Natural) = Natural x - y

    static member (-) (x : int64, y : Natural) = Natural x - y

    static member (-) (x : BigInteger, y : Natural) = Natural x - y

    static member (-) (x : float, y : Natural) = Natural x - y

    static member (-) (x : float32, y : Natural) = Natural x - y
    
    
    static member (*) (x : Natural, y : Natural) = Natural(x.IntVal * y.IntVal)

    static member (*) (x : Natural, y : int) = x * Natural y

    static member (*) (x : Natural, y : int64) = x * Natural y
    
    static member (*) (x : Natural, y : BigInteger) = x * Natural y

    static member (*) (x : Natural, y : float) = x * Natural y

    static member (*) (x : Natural, y : float32) = x * Natural y

    static member (*) (x : int, y : Natural) = Natural x * y
    
    static member (*) (x : int64, y : Natural) = Natural x * y
    
    static member (*) (x : BigInteger, y : Natural) = Natural x * y
    
    static member (*) (x : float, y : Natural) = Natural x * y
    
    static member (*) (x : float32, y : Natural) = Natural x * y

        
    static member (/) (x : Natural, y : Natural) = Rational(x.IntVal, y.IntVal)

    static member (/) (x : Natural, y : int) = x / Natural y

    static member (/) (x : Natural, y : int64) = x / Natural y

    static member (/) (x : Natural, y : BigInteger) = x / Natural y

    static member (/) (x : Natural, y : float) = x + Natural y

    static member (/) (x : Natural, y : float32) = x + Natural y

    static member (/) (x : int, y : Natural) = Rational(BigInteger(x), y.IntVal)

    static member (/) (x : int64, y : Natural) = Natural x / y

    static member (/) (x : BigInteger, y : Natural) = Natural x / y

    static member (/) (x : float, y : Natural) = Natural x / y

    static member (/) (x : float32, y : Natural) = Natural x / y    
    
    static member op_Explicit(r: Natural): int = (int) r.IntVal
    static member op_Explicit(r: Natural): float = (float) r.IntVal
    static member op_Explicit(r: Natural): float32 = (float32) r.IntVal
    static member op_Explicit(r: Natural): decimal = (decimal) r.IntVal 
    static member op_Explicit(r: Natural): BigInteger = r.IntVal
    static member op_Explicit(r: Natural): Rational = Rational r
    static member op_Explicit(r: Natural): MathNet.Numerics.BigRational = MathNet.Numerics.BigRational.FromBigInt r.IntVal
    static member op_Explicit(r: BigInteger): Natural = Natural r
    static member op_Explicit(r: int): Natural = Natural r   
    static member op_Explicit(r: float): Natural = Natural r
    
    static member op_Equality (l:Natural, r:Natural) = l.Equals r
    static member op_Inequality (l:Natural, r:Natural) = not <| l.Equals r

type real = float

type rat = Rational

type nat = Natural

type complex = System.Numerics.Complex

[<RequireQualifiedAccess>]
 module NumericLiteralQ = 
   let FromZero() = Rational.Zero
   let FromOne() = Rational.One
   let FromInt32 (i:int) = Rational(i, 1)
   let FromInt64 (i:int64) = Rational(i, 1L)

[<RequireQualifiedAccess>]
module NumericLiteralN = 
  let FromZero() = Natural.Zero
  let FromOne() = Natural.One
  let FromInt32 (i:int) = Natural i

[<ReflectedDefinition;AutoOpen>]
module Math =
    let (e:real) = Math.E
    
    let (pi:real) = Math.PI

    let factorial (n:int) :real = MathNet.Numerics.SpecialFunctions.Factorial n

    let binomial_coeff n r :real = (factorial n) / ((factorial r) * (factorial(n - r)))

    let erf (x:real) = MathNet.Numerics.SpecialFunctions.Erf x

[<AutoOpen>]
module Numbers =
    let real<'t when 't:equality and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable>(n:'t) :real = 
        match box n with
        | :? int as i -> float i
        | :? int64 as i -> float i
        | :? bigint as i -> float i
        | :? single as f -> float f
        | :? double as f -> float f
        | :? rat as r -> float r
        | :? Natural as n -> float n
        | _ -> failwithf "Cannot convert type %s to type real." typeof<'t>.Name

    let rat<'t when 't:equality and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable>(n:'t)  = 
        match box n with
        | :? int as i -> Rational(i, 1)
        | :? int64 as i -> Rational(i, 1L)
        | :? bigint as i -> Rational(i, bigint.One)
        | :? single as f -> Rational((double) f)
        | :? double as f -> Rational(f)
        | :? Natural as n -> Rational(n.IntVal)
        | _ -> failwithf "Cannot convert type %s to type Rational." typeof<'t>.Name

    let (|PosInf|_|) (e:Expr<'t>) =
        match e with
        | Int32 (Int32.MaxValue) -> Some()
        | Double (Double.MaxValue) -> Some()
        | _ -> None

    let (|NegInf|_|) (e:Expr<'t>) =
        match e with
        | Int32 (Int32.MinValue) -> Some()
        | Double (Double.MinValue) -> Some()
        | _ -> None

    [<ReflectedDefinition>]
    let inv n = n ** - 1.
     
    let is_perfect_square n =
        let h = n &&& 0xF
        if (h > 9) then false
        else
            if ( h <> 2 && h <> 3 && h <> 5 && h <> 6 && h <> 7 && h <> 8 ) then
                let t = ((n |> double |> sqrt) + 0.5) |> floor|> int
                t*t = n
            else false
    
    let inline (..+) (l:seq<'t>) (r:seq<'t>) = Seq.map2 (+) l r

    let inline (..-) (l:seq<'t>) (r:seq<'t>) = Seq.map2 (-) l r

    let inline (../) (l:seq<'t>) (r:seq<'t>) = Seq.map2 (/) l r

    let inline (..*) (l:seq<'t>) (r:seq<'t>) = Seq.map2 (*) l r

    let (|NumericConstant|_|) = 
        function
        | UInt16 x -> Expr.Value(x) |> Some 
        | UInt32 x -> Expr.Value(x) |> Some
        | UInt64 x -> Expr.Value(x) |> Some
        | Int16 x -> Expr.Value(x) |> Some
        | Int32 x -> Expr.Value(x) |> Some
        | Int64 x -> Expr.Value(x) |> Some
        | Decimal x -> Expr.Value(x) |> Some
        | Double x -> Expr.Value(x) |> Some
        | Decimal x -> Expr.Value(x) |> Some
        | _ -> None

    let (|RealType|_|): Type -> Type option =
        function
        | t when t = typeof<real> -> Some t
        | _ -> None

    let (|RationalType|_|): Type -> Type option =
        function
        | t when t = typeof<Rational> -> Some t
        | t when t = typeof<MathNet.Numerics.BigRational> -> Some t
        | _ -> None

    let (|NaturalType|_|): Type -> Type option =
        function
        | t when t = typeof<Natural> -> Some t
        | _ -> None

    let (|ComplexType|_|): Type -> Type option =
        function
        | t when t = typeof<Complex> -> Some t
        | _ -> None

    let (|Rational|_|) =
        function
        | Patterns.Value(v, RationalType _) -> Some (v :?> Rational)
        | _ -> None

    let (|Natural|_|) =
        function
        | Patterns.Value(v, NaturalType _) -> Some (v :?> Natural)
        | _ -> None

    let is_int:obj->bool = 
        function
        | :? int -> true
        | :? real as d -> d = Math.Floor(d + System.Double.Epsilon)
        | :? nat -> true
        | :? rat as r -> Rational.Normalize(r.Numerator, r.Denominator).Denominator = BigInteger.One
        | _ -> failwithf "unsupported"

    let rec to_int:obj->int =
        function
        | :? real as r -> System.Convert.ToInt32(r)
        | :? rat as q -> q |> real |> to_int
        | :? nat as n -> (int) n
        | x -> failwithf "Cannot convert %A to an integer." x

    let to_real (o:'a) : real = System.Convert.ToDouble o

    let real_seq<'t when 't:equality and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable> (s:seq<'t>) = s |> Seq.map real

    let real_frac (n:int) (d:int) = (real) n / (real) d

    let real_frac_mi = <@ real_frac 1 1 @> |> function | FSharp.Quotations.Patterns.Call(_, mi, _) -> mi | _ -> failwith ""