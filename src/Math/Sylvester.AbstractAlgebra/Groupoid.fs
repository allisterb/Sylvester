namespace Sylvester

open System.Collections

open Sylvester.Arithmetic.N10
open Sylvester.Collections

/// Set of elements closed under some binary operation.
type IGroupoid<'t when 't: equality> = 
    inherit IStruct<'t, card.one>
    inherit Generic.IEnumerable<'t * 't * 't>
    abstract member Op: BinaryOp<'t>
    
/// Set of elements closed under some binary operation.
type Groupoid<'t when 't: equality>(set:Set<'t>, op:BinaryOp<'t>) =
    inherit Struct<'t, card.one>(set, arrayOf1 (Binary(op)))
    member val Op = op
    member x.Item(l:'t, r:'t) = x.Op l r
    member x.ToArray n = x |> Seq.take n |> Seq.toArray
    interface IGroupoid<'t> with
        member val Op = op
        member x.GetEnumerator(): Generic.IEnumerator<'t * 't * 't> = (let s = x.Set :> Generic.IEnumerable<'t> in s |> Seq.pairwise |> Seq.map (fun(a, b) -> (a, b, (op) a b))).GetEnumerator()
        member x.GetEnumerator(): IEnumerator = (x :> Generic.IEnumerable<'t * 't * 't>).GetEnumerator () :> IEnumerator

/// Category of groupoids with a structure-preserving morphism.
type Groupoids<'t when 't : equality>(l:Groupoid<'t>, r:Groupoid<'t>, map: Map<'t>) = 
    inherit Category<'t, Groupoid<'t>, card.one>((Morph(l, r, map)))
    member x.Map = map
    member x.Item(e:'t) = map e 

    