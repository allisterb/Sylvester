namespace Sylvester

open System

open Arithmetic

/// Set of n-ary elements called vectors over a field of scalars with 2 left-associative, commutative, operations on vectors. 
type IVectorSpace<'n, 't when 'n :> Number and 't: equality and 't: comparison and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable> =  
    inherit ISet<Vector<'n, 't>>
    abstract Dim0:'n
    abstract Field:IField<'t>
    abstract Op:BinaryOp<Vector<'n, 't>>
    abstract Op2:BinaryOp<'t, Vector<'n, 't>>
    
/// Vector space with an additional operation called inner product mapping vectors to scalars.
type IInnerProductSpace<'n, 't, 'v when 'n :> Number and 't: equality and 't:comparison and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable> =  
    inherit IVectorSpace<'n, 't>
    abstract Op3:BinaryOp<Vector<'n, 't>, 't>

/// Set of elements closed under a left-associative commutative operations and a 2nd left-associative distributive operation.
type VectorSpace<'n, 't when 'n :> Number and 't: equality and 't: comparison and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable>
    (field: IField<'t>, op: BinaryOp<Vector<'n, 't>>, op2: BinaryOp<'t, Vector<'n, 't>>) =
    interface IVectorSpace<'n, 't> with
        member val Set = Set.U
        member x.Equals (y:Set<Vector<'n, 't>>) = x.Equals y
        member val Field = field
        member val Dim0 = number<'n>
        member val Op = op
        member val Op2 = op2

type InnerProductSpace<'n, 't when 'n :> Number and 't: equality and 't: comparison and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable>
    (field: IField<'t>, op: BinaryOp<Vector<'n, 't>>, op2: BinaryOp<'t, Vector<'n, 't>>, op3: BinaryOp<Vector<'n, 't>, 't>) = 
    inherit VectorSpace<'n, 't>(field, op, op2)
    interface IInnerProductSpace<'n, 't, Vector<'n, 't>> with
        member val Op3 = op3