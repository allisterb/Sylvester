namespace Sylvester

open System.Collections

open Sylvester.Arithmetic
open Sylvester.Collections

/// Set of elements closed under some binary operation.
type ILattice<'t when 't: equality> = 
    inherit ISet<'t>
    inherit Generic.IEnumerable<'t * 't * 't>
    abstract member Join: BinaryOp<'t>
    abstract member Meet: BinaryOp<'t>

type Lattice<'t when 't: equality>(set: ISet<'t>, join: BinaryOp<'t>, meet: BinaryOp<'t>) =
    inherit Struct<'t, card.two>(set, arrayOf2 (Binary(join)) (Binary(meet)))
