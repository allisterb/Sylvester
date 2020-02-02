namespace Sylvester

open System.Collections

open Sylvester.Collections

/// Set of elements with an order relation.
type IPoset<'t when 't: equality and 't: comparison> = 
    inherit ISet<'t>
    inherit Generic.IEnumerable<'t * 't * bool>
    abstract member Op: Order<'t>
    
/// Set of elements with an order relation.
type Poset<'t when 't: equality and 't: comparison>(set:ISet<'t>, op:Order<'t>) = 
    inherit Struct<'t, card.one>(set, arrayOf1 (Order(op)))
    member val Op = op
    member x.Item(l:'t, r:'t) = x.Op l r
    member x.ToArray n = x |> Seq.take n |> Seq.toArray
    interface IPoset<'t> with
        member val Set = set.Set
        member val Op = op
        member x.GetEnumerator(): Generic.IEnumerator<'t * 't * bool> = 
            (let s = x.Set :> Generic.IEnumerable<'t> in s |> Seq.pairwise |> Seq.map (fun(a, b) -> (a, b, (op) a b))).GetEnumerator()
        member x.GetEnumerator(): IEnumerator = (x :> Generic.IEnumerable<'t * 't * bool>).GetEnumerator () :> IEnumerator

