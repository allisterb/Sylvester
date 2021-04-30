namespace Sylvester

open System
open System.Collections  

open Sylvester.Arithmetic
open Sylvester.Collections

/// Set of n-ary elements called vectors over a field of scalars with 2 left-associative, commutative, operations on vectors. 
type IVectorSpace<'n, 't, 'v when 'v: equality and 'n :> Number and 't: equality and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable> =  
    inherit ISet<'v>
    abstract Dim0:'n
    abstract Field:IField<'t>
    abstract Op:BinaryOp<'v>
    abstract Op2:BinaryOp<'t, 'v>
    
/// Set of elements closed under a left-associative commutative operations and a 2nd left-associative distributive operation.
type VectorSpace<'n, 't, 'v when 'v: equality and 'n :> Number and 't: equality and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable>
    (field: IField<'t>, op: BinaryOp<'v>, op2: BinaryOp<'t, 'v>) =
    member val Field = field
    member val Op = op
    member val Op2 = op2
    interface IVectorSpace<'n, 't, 'v> with
        member val Set = (set' pos_inf<'v> pos_inf<'v>).Set 
        member x.Equals (y:Set<'v>) = x.Equals y
        member val Field = field
        member val Dim0 = number<'n>
        member val Op = op
        member val Op2 = op2