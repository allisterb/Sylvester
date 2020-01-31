namespace Sylvester

open System.Collections

open Sylvester.Arithmetic
open Sylvester.Collections

/// Set of elements closed under some binary operation.
type IGroupoid<'t when 't: equality> = 
    inherit ISet<'t>
    inherit Generic.IEnumerable<'t * 't * 't>
    abstract member Op: BinaryOp<'t>
    
/// Set of elements closed under some binary operation.
type Groupoid<'t when 't: equality>(set:Set<'t>, op:BinaryOp<'t>) =
    inherit Struct<'t, card.one>(set, arrayOf1 (Binary(op)))
    member val Op = op
    member x.Item(l:'t, r:'t) = x.Op l r
    member x.ToArray n = x |> Seq.take n |> Seq.toArray
    interface IGroupoid<'t> with
        member val Set = set
        member val Op = op
        member x.GetEnumerator(): Generic.IEnumerator<'t * 't * 't> = 
            (let s = x.Set :> Generic.IEnumerable<'t> in s |> Seq.pairwise |> Seq.map (fun(a, b) -> (a, b, (op) a b))).GetEnumerator()
        member x.GetEnumerator(): IEnumerator = (x :> Generic.IEnumerable<'t * 't * 't>).GetEnumerator () :> IEnumerator

/// Category of groupoids with n structure-preserving morphisms.
type Groupoids<'ut, 'vt, 'n when 'ut : equality and 'vt: equality and 'n :> Number>(l:Groupoid<'ut>, r:Groupoid<'vt>, maps: Array<'n, Map<'ut, 'vt>>) = 
    inherit Category<'ut, 'vt, card.one, card.one, 'n>(l, r, maps)