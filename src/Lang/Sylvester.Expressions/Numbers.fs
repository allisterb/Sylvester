#nowarn "0077"

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
        new(p:int, q:int) = {Numerator = BigInteger p; Denominator = BigInteger q}
        new(p:int64, q:int64) = {Numerator = BigInteger p; Denominator = BigInteger q}
        new(p:float, q:float) = {Numerator = BigInteger p; Denominator = BigInteger q}
        new(p:float32, q:float32) = {Numerator = BigInteger p; Denominator = BigInteger q}
        new(x: float) = // From: http://www.fssnip.net/kV/title/Convert-a-Float-to-a-Mixed-Number
            let wholePart = int x     // whole part of x
            let decimalPt = x % 1.0   // decimal part of x
            let rec cF(Z : float, i : int, Dm : float, Do : float) =
                match Z % 1.0 > 1e-6, i < 1 with
                //  First case terminates after 14 iterations
                | _    , true  -> (wholePart, (int64 (System.Math.Round(decimalPt * Do)), int64 Do))
                //  Second case executes next cF (continuing fraction)
                | true , false -> cF(1.0/(Z % 1.0), i - 1 , Do, Do * System.Math.Round(1.0/(Z % 1.0)-0.5) + Dm )
                //  Final case terminates if the remainder of Z > 10^-6
                | false, _  -> (wholePart, (int64 (System.Math.Round(decimalPt * Do)), int64 Do))
            let w, (n, d) = decimalPt |> fun x -> cF(x, 14, 0.0, 1.0)
            let n' = ((w |> int64) * d) + n
            { Numerator = n' |> BigInteger; Denominator = d |> BigInteger }
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

    static member Reciprocal (num : Rational) = Rational.Normalize (num.Denominator, num.Numerator)

    static member Pow (num : Rational, n : int) =
        if n < 0 then 
            Rational.Normalize (BigInteger.Pow (num.Denominator, -n), BigInteger.Pow (num.Numerator, -n))
        else 
            Rational (BigInteger.Pow (num.Numerator, n), BigInteger.Pow (num.Denominator, n))
        
    static member Pow (num : Rational, pow : Rational) =
        let g, p = (float) num,  (float) pow
        Rational(g ** p)

    static member Abs(r: Rational) = Rational((abs r.Numerator), (abs r.Denominator))

    static member Sqrt(r: Rational) = 
        let n, d = (float) r.Numerator, (float) r.Denominator
        Rational((sqrt n), (sqrt d))
    
    static member (~+) (r : Rational) = r

    static member (~-) (num : Rational) = Rational(-num.Numerator, num.Denominator)

    static member (+) (x : Rational, y : Rational) = Rational.Normalize ((x.Numerator * y.Denominator) + (y.Numerator * x.Denominator), x.Denominator * y.Denominator)

    static member (+) (x : Rational, y : int) = let y' = Rational (y, 1) in Rational.Normalize ((x.Numerator * y'.Denominator) + (y'.Numerator * x.Denominator), x.Denominator * y'.Denominator)

    static member (+) (x : Rational, y : int64) = let y' = Rational (y, 1L) in Rational.Normalize ((x.Numerator * y'.Denominator) + (y'.Numerator * x.Denominator), x.Denominator * y'.Denominator)
    
    static member (+) (x : Rational, y : BigInteger) = let y' = Rational (y, BigInteger.One) in Rational.Normalize ((x.Numerator * y'.Denominator) + (y'.Numerator * x.Denominator), x.Denominator * y'.Denominator)

    static member (+) (x : Rational, y : float) = let y' = Rational (y, 1.) in Rational.Normalize ((x.Numerator * y'.Denominator) + (y'.Numerator * x.Denominator), x.Denominator * y'.Denominator)

    static member (+) (x : Rational, y : float32) = let y' = Rational (y, 1.0f) in Rational.Normalize ((x.Numerator * y'.Denominator) + (y'.Numerator * x.Denominator), x.Denominator * y'.Denominator)

    static member (+) (x : int, y : Rational) = let x' = Rational (x, 1) in Rational.Normalize ((x'.Numerator * y.Denominator) + (y.Numerator * x'.Denominator), x'.Denominator * y.Denominator)
    
    static member (+) (x : int64, y : Rational) = let x' = Rational (x, 1L) in Rational.Normalize ((x'.Numerator * y.Denominator) + (y.Numerator * x'.Denominator), x'.Denominator * y.Denominator)
    
    static member (+) (x : BigInteger, y : Rational) = let x' = Rational (x, BigInteger.One) in Rational.Normalize ((x'.Numerator * y.Denominator) + (y.Numerator * x'.Denominator), x'.Denominator * y.Denominator)
    
    static member (+) (x : float, y : Rational) = let x' = Rational (x, 1.) in Rational.Normalize ((x'.Numerator * y.Denominator) + (y.Numerator * x'.Denominator), x'.Denominator * y.Denominator)
    
    static member (+) (x : float32, y : Rational) = let x' = Rational (x, 1.0f) in Rational.Normalize ((x'.Numerator * y.Denominator) + (y.Numerator * x'.Denominator), x'.Denominator * y.Denominator)
    

    static member (-) (x : Rational, y : Rational) = Rational.Normalize ((x.Numerator * y.Denominator) - (y.Numerator * x.Denominator), x.Denominator * y.Denominator)
    
    static member (-) (x : Rational, y : int) = let y' = Rational (y, 1) in Rational.Normalize ((x.Numerator * y'.Denominator) - (y'.Numerator * x.Denominator), x.Denominator * y'.Denominator)

    static member (-) (x : Rational, y : int64) = let y' = Rational (y, 1L) in Rational.Normalize ((x.Numerator * y'.Denominator) - (y'.Numerator * x.Denominator), x.Denominator * y'.Denominator)
    
    static member (-) (x : Rational, y : BigInteger) = let y' = Rational (y, BigInteger.One) in Rational.Normalize ((x.Numerator * y'.Denominator) - (y'.Numerator * x.Denominator), x.Denominator * y'.Denominator)

    static member (-) (x : Rational, y : float) = let y' = Rational (y, 1.) in Rational.Normalize ((x.Numerator * y'.Denominator) - (y'.Numerator * x.Denominator), x.Denominator * y'.Denominator)

    static member (-) (x : Rational, y : float32) = let y' = Rational (y, 1.0f) in Rational.Normalize ((x.Numerator * y'.Denominator) - (y'.Numerator * x.Denominator), x.Denominator * y'.Denominator)

    static member (-) (x : int, y : Rational) = let x' = Rational (x, 1) in Rational.Normalize ((x'.Numerator * y.Denominator) - (y.Numerator * x'.Denominator), x'.Denominator * y.Denominator)
    
    static member (-) (x : int64, y : Rational) = let x' = Rational (x, 1L) in Rational.Normalize ((x'.Numerator * y.Denominator) - (y.Numerator * x'.Denominator), x'.Denominator * y.Denominator)
    
    static member (-) (x : BigInteger, y : Rational) = let x' = Rational (x, BigInteger.One) in Rational.Normalize ((x'.Numerator * y.Denominator) - (y.Numerator * x'.Denominator), x'.Denominator * y.Denominator)
    
    static member (-) (x : float, y : Rational) = let x' = Rational (x, 1.) in Rational.Normalize ((x'.Numerator * y.Denominator) - (y.Numerator * x'.Denominator), x'.Denominator * y.Denominator)
    
    static member (-) (x : float32, y : Rational) = let x' = Rational (x, 1.0f) in Rational.Normalize ((x'.Numerator * y.Denominator) - (y.Numerator * x'.Denominator), x'.Denominator * y.Denominator)
    
    
    static member (*) (x : Rational, y : Rational) = Rational.Normalize (x.Numerator * y.Numerator, x.Denominator * y.Denominator)
    
    static member (*) (x : Rational, y : int) = let y' = Rational (y, 1) in Rational.Normalize ((x.Numerator * y'.Denominator) * (y'.Numerator * x.Denominator), x.Denominator * y'.Denominator)

    static member (*) (x : Rational, y : int64) = let y' = Rational (y, 1L) in Rational.Normalize ((x.Numerator * y'.Denominator) * (y'.Numerator * x.Denominator), x.Denominator * y'.Denominator)
    
    static member (*) (x : Rational, y : BigInteger) = let y' = Rational (y, BigInteger.One) in Rational.Normalize ((x.Numerator * y'.Denominator) * (y'.Numerator * x.Denominator), x.Denominator * y'.Denominator)

    static member (*) (x : Rational, y : float) = let y' = Rational (y, 1.) in Rational.Normalize ((x.Numerator * y'.Denominator) * (y'.Numerator * x.Denominator), x.Denominator * y'.Denominator)

    static member (*) (x : Rational, y : float32) = let y' = Rational (y, 1.0f) in Rational.Normalize ((x.Numerator * y'.Denominator) * (y'.Numerator * x.Denominator), x.Denominator * y'.Denominator)

    static member (*) (x : int, y : Rational) = let x' = Rational (x, 1) in Rational.Normalize ((x'.Numerator * y.Denominator) * (y.Numerator * x'.Denominator), x'.Denominator * y.Denominator)
    
    static member (*) (x : int64, y : Rational) = let x' = Rational (x, 1L) in Rational.Normalize ((x'.Numerator * y.Denominator) * (y.Numerator * x'.Denominator), x'.Denominator * y.Denominator)
    
    static member (*) (x : BigInteger, y : Rational) = let x' = Rational (x, BigInteger.One) in Rational.Normalize ((x'.Numerator * y.Denominator) * (y.Numerator * x'.Denominator), x'.Denominator * y.Denominator)
    
    static member (*) (x : float, y : Rational) = let x' = Rational (x, 1.) in Rational.Normalize ((x'.Numerator * y.Denominator) * (y.Numerator * x'.Denominator), x'.Denominator * y.Denominator)
    
    static member (*) (x : float32, y : Rational) = let x' = Rational (x, 1.0f) in Rational.Normalize ((x'.Numerator * y.Denominator) * (y.Numerator * x'.Denominator), x'.Denominator * y.Denominator)
    
    
    static member (/) (x : Rational, y : Rational) = Rational.Normalize (x.Numerator * y.Denominator, x.Denominator * y.Numerator)
    
    static member (/) (x : Rational, y : int) = let y' = Rational (y, 1) in Rational.Normalize ((x.Numerator * y'.Denominator) / (y'.Numerator * x.Denominator), x.Denominator * y'.Denominator)

    static member (/) (x : Rational, y : int64) = let y' = Rational (y, 1L) in Rational.Normalize ((x.Numerator * y'.Denominator) / (y'.Numerator * x.Denominator), x.Denominator * y'.Denominator)
    
    static member (/) (x : Rational, y : BigInteger) = let y' = Rational (y, BigInteger.One) in Rational.Normalize ((x.Numerator * y'.Denominator) / (y'.Numerator * x.Denominator), x.Denominator * y'.Denominator)

    static member (/) (x : Rational, y : float) = let y' = Rational (y, 1.) in Rational.Normalize ((x.Numerator * y'.Denominator) / (y'.Numerator * x.Denominator), x.Denominator * y'.Denominator)

    static member (/) (x : Rational, y : float32) = let y' = Rational (y, 1.0f) in Rational.Normalize ((x.Numerator * y'.Denominator) / (y'.Numerator * x.Denominator), x.Denominator * y'.Denominator)

    static member (/) (x : int, y : Rational) = let x' = Rational (x, 1) in Rational.Normalize ((x'.Numerator * y.Denominator) / (y.Numerator * x'.Denominator), x'.Denominator * y.Denominator)
    
    static member (/) (x : int64, y : Rational) = let x' = Rational (x, 1L) in Rational.Normalize ((x'.Numerator * y.Denominator) / (y.Numerator * x'.Denominator), x'.Denominator * y.Denominator)
    
    static member (/) (x : BigInteger, y : Rational) = let x' = Rational (x, BigInteger.One) in Rational.Normalize ((x'.Numerator * y.Denominator) / (y.Numerator * x'.Denominator), x'.Denominator * y.Denominator)
    
    static member (/) (x : float, y : Rational) = let x' = Rational (x, 1.) in Rational.Normalize ((x'.Numerator * y.Denominator) / (y.Numerator * x'.Denominator), x'.Denominator * y.Denominator)
    
    static member (/) (x : float32, y : Rational) = let x' = Rational (x, 1.0f) in Rational.Normalize ((x'.Numerator * y.Denominator) / (y.Numerator * x'.Denominator), x'.Denominator * y.Denominator)
    
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
    static member op_Explicit(r: BigInteger): Rational = Rational(r)
    static member op_Explicit(r: int): Rational = Rational(r, 1)   
    static member op_Explicit(r: float): Rational = Rational(r)
    static member op_Equality (l:Rational, r:Rational) = l.Equals r
    static member op_Inequality (l:Rational, r:Rational) = not <| l.Equals r

type real = float

type rat = Rational

[<RequireQualifiedAccess>]
 module NumericLiteralQ = 
   let FromZero() = Rational.Zero
   let FromOne() = Rational.One
   let FromInt32 (i:int) = Rational(i, 1)
   let FromInt64 (i:int64) = Rational(i, 1L)

[<ReflectedDefinition;AutoOpen>]
module Math =
    let (e:real) = Math.E
    
    let (pi:real) = Math.PI

    let factorial (n:int) = MathNet.Numerics.SpecialFunctions.Factorial n

    let binomial_coeff n r = (factorial n) / ((factorial r) * (factorial(n - r)))

[<AutoOpen>]
module Numbers =
    let real n :real = float n

    let rat<'a when 'a: struct>(n:'a)  = 
        match box n with
        | :? int as i -> Rational(i, 1)
        | :? int64 as i -> Rational(i, 1L)
        | :? bigint as i -> Rational(i, bigint.One)
        | :? single as f -> Rational(f, 1.0f)
        | :? double as f -> Rational(f, 1.)
        | _ -> failwithf "Cannot convert type %s to type Rational." typeof<'a>.Name

    let pos_inf<'t> = Unchecked.defaultof<'t>

    let neg_inf<'t> = Unchecked.defaultof<'t>
    
    let (^) (l:'t) (r:'t when 't: equality and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IConvertible) =
        let l', r' = System.Convert.ToDouble l, System.Convert.ToDouble r in
        System.Convert.ChangeType(l' ** r', typeof<'t>) :?> 't

    let inline inv n = n ** - 1.
     
    let inline zero (x : ^T) = (^T : (member Zero : 't) (x))

    let inline one (x : ^T) = (^T : (member One : 't) (x))
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

    let (|BigRationalType|_|): Type -> Type option =
        function
        | t when t.Name = "BigRational" -> Some t
        | _ -> None
    let (|BigIntegerType|_|): Type -> Type option =
        function
        | t when t.Name = "BigInteger" -> Some t
        | _ -> None
    let (|RationalType|_|): Type -> Type option =
        function
        | t when t.Name = "Rational" -> Some t
        | _ -> None

    let (|ComplexType|_|): Type -> Type option =
        function
        | t when t = typeof<Complex> -> Some t
        | _ -> None

    let (|Rational|_|) =
        function
        | Patterns.Value(v, RationalType _) -> Some (v :?> Rational)
        | _ -> None