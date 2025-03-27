namespace Sylvester

open System


/// Set of n-ary elements called vectors over a field of scalars with 2 left-associative, commutative, operations on vectors. 
type IVectorSpace<'t when 't: equality and 't: comparison and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable> =  
    inherit ISet<Vector<'t>>
    abstract Dim0:int
    abstract Field:IField<'t>
    abstract Op:BinaryOp<Vector<'t>>
    abstract Op2:BinaryOp<Scalar<'t>, Vector<'t>>
    
/// Vector space with an additional operation called inner product mapping vectors to scalars.
type IInnerProductSpace<'t, 'v when 't: equality and 't:comparison and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable> =  
    inherit IVectorSpace<'t>
    abstract Op3:BinaryOp<Vector<'t>, Scalar<'t>>

/// Set of elements closed under a left-associative commutative operations and a 2nd left-associative distributive operation.
type VectorSpace<'t when 't: equality and 't: comparison and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable>
    (n:int, field: IField<'t>, op: BinaryOp<Vector<'t>>, op2: BinaryOp<Scalar<'t>, Vector<'t>>) =
    interface IVectorSpace<'t> with
        member val Set = Set.U
        member x.Equals (y:Set<Vector<'t>>) = x.Equals y
        member val Field = field
        member val Dim0 = n
        member val Op = op
        member val Op2 = op2

type InnerProductSpace<'t when 't: equality and 't: comparison and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable>
    (n:int, field: IField<'t>, op: BinaryOp<Vector<'t>>, op2: BinaryOp<Scalar<'t>, Vector<'t>>, op3: BinaryOp<Vector<'t>, Scalar<'t>>) = 
    inherit VectorSpace<'t>(n, field, op, op2)
    interface IInnerProductSpace<'t, Vector<'t>> with
        member val Op3 = op3