namespace Sylvester

open System
open System.Collections  

open Sylvester.Arithmetic
open Sylvester.Collections

/// Set of n-ary elements over a field called vectors with 2 left-associative, commutative, operations on vectors. 
type IVectorSpace<'n, 't when 'n :> Number and 't: equality and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable> =  
    inherit IField<'t>
    abstract Dim0:'n
    abstract member Op:BinaryOp<IVector<'n, 't>>
    abstract member Op2:BinaryOp<IVector<'n, 't>>
and IVector<'n, 't when 'n :> Number and 't: equality and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable> =
    abstract Dim0:'n
    abstract Item:int->'t

/// Set of elements closed under a left-associative commutative operations and a 2nd left-associative distributive operation.
type VectorSpace<'n, 't when 'n :> Number and 't: equality and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable>
    (field: IField<'t>, op:BinaryOp<IVector<'n, 't>>, op2:BinaryOp<IVector<'n, 't>>) =
    inherit Struct<'t, card.six>(field.Set, arrayOf6 (Binary(field.AddGroup.Op)) (Nullary(field.AddGroup.Identity)) (Unary(field.AddGroup.Inverse)) (Binary(field.AddGroup.Op)) (Nullary(field.AddGroup.Identity)) (Unary(field.AddGroup.Inverse)))
    member val Op = op
    member val Op2 = op2
    interface IVectorSpace<'n, 't> with
        member val Set = field.AddGroup.Set
        member val AddGroup = field.AddGroup
        member val MulGroup = field.MulGroup
        member val Dim0 = number<'n>
        member val Op = op
        member val Op2 = op2

type R<'n when 'n :> Number> = IVector<'n, real>