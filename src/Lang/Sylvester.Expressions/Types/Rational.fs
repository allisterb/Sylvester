namespace Sylvester

open System
open System.Numerics

[<CustomEquality; CustomComparison>]
type Rational =
    struct 
        val Numerator: BigInteger
        val Denominator: BigInteger
        new(p:BigInteger, q:BigInteger) = {Numerator = p; Denominator = q}
        new(p:BigInteger) = {Numerator = p; Denominator = BigInteger.One}
        new(p:int, q:int) = {Numerator = BigInteger p; Denominator = BigInteger q}
        new(p:int64, q:int64) = {Numerator = BigInteger p; Denominator = BigInteger q}
        new(p:float, q:float) = {Numerator = BigInteger p; Denominator = BigInteger q}
        new(p:float32, q:float32) = {Numerator = BigInteger p; Denominator = BigInteger q}
    end 
    member x.Tuple: ValueTuple<BigInteger, BigInteger> = x.Numerator, x.Denominator
    member x.Equals(y: Rational) = x.Tuple.Equals y.Tuple
    override x.Equals (y:obj) = 
        match y with 
        | :? Rational as r -> x.Equals r 
        | :? BigInteger as i -> Rational(i, BigInteger.One) |> x.Equals
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

    static member (~+) (r : Rational) = r

    static member (~-) (num : Rational) = Rational(-num.Numerator, num.Denominator)

    static member (+) (x : Rational, y : Rational) = Rational.Normalize ((x.Numerator * y.Denominator) + (y.Numerator * x.Denominator), x.Denominator * y.Denominator)

    static member (-) (x : Rational, y : Rational) = Rational.Normalize ((x.Numerator * y.Denominator) - (y.Numerator * x.Denominator), x.Denominator * y.Denominator)

    static member (*) (x : Rational, y : Rational) = Rational.Normalize (x.Numerator * y.Numerator, x.Denominator * y.Denominator)

    static member (/) (x : Rational, y : Rational) = Rational.Normalize (x.Numerator * y.Denominator, x.Denominator * y.Numerator)
    
    static member op_Explicit(r: Rational): float = (float) r.Numerator / (float) r.Denominator
    static member op_Explicit(r: Rational): float32 = (float32) r.Numerator / (float32) r.Denominator
    static member op_Equality (l:Rational, r:Rational) = l.Equals r
    static member op_Inequality (l:Rational, r:Rational) = not <| l.Equals r

type Q = Rational

[<RequireQualifiedAccess>]
 module NumericLiteralZ = 
   let FromZero () = Rational.Zero
   let FromOne  () = Rational.One 
   let FromInt32 (i:int) = Rational(i, 1)
   let FromInt64 (i:int64) = Rational(i, 1L)