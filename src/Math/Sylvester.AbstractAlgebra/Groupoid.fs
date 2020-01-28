namespace Sylvester

open System.Collections

open Sylvester.Arithmetic.N10
open Sylvester.Collections

/// Set of elements closed under some binary operation.
type IGroupoid<'U when 'U: equality> = 
    inherit IStruct<'U, card.one>
    inherit Generic.IEnumerable<'U * 'U * 'U>
    abstract member Op: BinaryOp<'U>
    
/// Set of elements closed under some binary operation.
type Groupoid<'U when 'U: equality>(set:Set<'U>, op:BinaryOp<'U>) =
    inherit Struct<'U, card.one>(set, arrayOf1 (Binary(op)))
    member val Op = op
    member x.Item(l:'U, r:'U) = x.Op l r
    interface IGroupoid<'U> with
        member val Op = op
        member x.GetEnumerator(): Generic.IEnumerator<'U * 'U * 'U> = (let s = x.Set :> Generic.IEnumerable<'U> in s |> Seq.pairwise |> Seq.map (fun(a, b) -> (a, b, (op) a b))).GetEnumerator()
        member x.GetEnumerator(): IEnumerator = (x :> Generic.IEnumerable<'U * 'U * 'U>).GetEnumerator () :> IEnumerator

/// Category of groups with a structure-preserving morphism.
type Groupoids<'U when 'U : equality>(l:Groupoid<'U>, r:Groupoid<'U>, map: Map<'U>) = 
    inherit Category<'U, Groupoid<'U>, card.one, card.one>(arrayOf1 (Morph(l, r, map)))
    member x.Map = map
    member x.Morph = x.Morphisms.[zero]
    member x.Item(e:'U) = map e 

    