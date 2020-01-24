namespace Sylvester

open System

type Numeric<'t when 't:> ValueType and 't : struct  and 't: (new: unit -> 't) 
    and 't : (static member (+) : 't * 't -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable> =
    class end