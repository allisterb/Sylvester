namespace Sylvester

open System.Collections
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
    interface IGroupoid<'U> with
        member val Op = op
        member x.GetEnumerator(): Generic.IEnumerator<'U * 'U * 'U> = (let s = x.Set :> Generic.IEnumerable<'U> in s |> Seq.pairwise |> Seq.map (fun(a, b) -> (a, b, (op) a b))).GetEnumerator()
        member x.GetEnumerator(): IEnumerator = (x :> Generic.IEnumerable<'U * 'U * 'U>).GetEnumerator () :> IEnumerator
    