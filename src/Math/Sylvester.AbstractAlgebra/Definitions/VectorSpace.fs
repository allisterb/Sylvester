namespace Sylvester

open System
open System.Collections  

open Sylvester.Arithmetic
open Sylvester.Collections

/// Set of n-ary elements called vectors over a field of scalars with 2 left-associative, commutative, operations on vectors. 
type IVectorSpace<'n, 't, 'v when 'v: equality and 'n :> Number and 't: equality and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable> =  
    abstract Dim0:'n
    abstract Field:IField<'t>
    abstract Op:BinaryOp<'v>
    abstract Op2:BinaryOp<'v>
    
/// Set of elements closed under a left-associative commutative operations and a 2nd left-associative distributive operation.
type VectorSpace<'n, 't, 'v when 'v: equality and 'n :> Number and 't: equality and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable>
    (field: IField<'t>, op: BinaryOp<'v>, op2: BinaryOp<'v>) =
    inherit Struct<'t, card.six>(field.Set, arrayOf6 (Binary(field.AddGroup.Op)) (Nullary(field.AddGroup.Identity)) (Unary(field.AddGroup.Inverse)) (Binary(field.AddGroup.Op)) (Nullary(field.AddGroup.Identity)) (Unary(field.AddGroup.Inverse)))
    member val Op = op
    member val Op2 = op2
    interface IVectorSpace<'n, 't, 'v> with
        member val Field = field
        member val Dim0 = number<'n>
        member val Op = op
        member val Op2 = op2