namespace Sylvster

open System

[<CustomEquality; CustomComparison>]
type Rational =
    struct
        val Numerator: int64
        val Denominator: int64
        new(p:int64, q:int64) = {Numerator = p; Denominator = q}
    end 
    member x.Tuple: ValueTuple<int64, int64> = x.Numerator, x.Denominator
    member x.Equals(y: Rational) = x.Tuple.Equals y.Tuple
    override x.Equals (y:obj) = match y with | :? Rational as r -> x.Equals r | _ -> false
    override x.GetHashCode () = x.Tuple.GetHashCode()
    override x.ToString () = if x.Denominator = 1L then x.Numerator.ToString () else sprintf "%i/%i" x.Numerator x.Denominator
    interface IEquatable<Rational> with member a.Equals b = a.Tuple = b.Tuple
    interface IComparable<Rational> with
        member x.CompareTo y = (x.Numerator * y.Denominator).CompareTo(y.Numerator * x.Denominator)
