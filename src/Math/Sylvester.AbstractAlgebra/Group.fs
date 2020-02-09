namespace Sylvester

open System.Collections

open Sylvester.Arithmetic
open Sylvester.Collections

[<StructuredFormatDisplay("e{Index}")>]
type GroupElement<'order when 'order :> Number> =
    struct
        val Index: int
        new(i:int) = {Index = i}
    end
    with 
        static member Identity = GroupElement<'order>(0)
        static member op_Explicit (e:GroupElement<'order>) :int = e.Index
        static member (*) (l:GroupElement<'order>,r:GroupElement<'order>) = 
            let a = (int) l
            let b = (int) r
            let m = number<'order>.IntVal
            GroupElement<'order>(Mod.(*) m a b)
   
/// Set of elements closed under some left-associative operation with identity and an inverse unary operation.
type IGroup<'t when 't: equality> = 
    inherit IMonoid<'t> 
    inherit IInverse<'t>

type IAbelianGroup<'t when 't: equality> = inherit IGroup<'t> 

type IAdditiveGroup<'t when 't: equality> = inherit IAbelianGroup<'t>

type IMultiplicativeGroup<'t when 't: equality> = inherit IAbelianGroup<'t>

/// Set of elements closed under some left-associative operation with identity and an inverse unary operation.
type Group<'t when 't: equality>(set:ISet<'t>, op:BinaryOp<'t>, ident:'t, inv: UnaryOp<'t>) =
    inherit Struct<'t, card.three>(set, arrayOf3 (Binary(op)) (Nullary(ident)) (Unary(inv)))    
    member val Op = op
    member val Identity = ident
    member val Inverse = inv
    interface IGroup<'t> with 
        member val Op = op
        member val Identity = ident
        member val Inverse = inv
        member x.GetEnumerator(): Generic.IEnumerator<'t * 't * 't> = 
            (let s = x.Set :> Generic.IEnumerable<'t> in s |> Seq.pairwise |> Seq.map (fun(a, b) -> (a, b, (op) a b))).GetEnumerator()
        member x.GetEnumerator(): IEnumerator = (x :> Generic.IEnumerable<'t * 't * 't>).GetEnumerator () :> IEnumerator

type AbelianGroup<'t when 't: equality>(set:ISet<'t>, op: BinaryOp<'t>, id:'t, inv: UnaryOp<'t>) =
    inherit Group<'t>(set, op, id, inv)
    do failIfNotCommutative op
    interface IAbelianGroup<'t>

type FiniteGroup<'order, 't when 'order :> Number and 't: equality>(set:FiniteSet<'order, 't>, op: BinaryOp<'t>, id:'t, inv: UnaryOp<'t>) =
    inherit Group<'t>(set, op, id, inv)
    member x.El0<'n when 'n :> card.one>() = (x, GroupElement<'order>(0))
    member x.El1<'n when 'n :> card.two>() = (x, GroupElement<'order>(0), GroupElement<'order>(1))

type FiniteAbelianGroup<'order, 't when 'order :> Number and 't: equality>(set:FiniteSet<'order, 't>, op: BinaryOp<'t>, id:'t, inv: UnaryOp<'t>) =
    inherit FiniteGroup<'order, 't>(set, op, id, inv)
    do failIfNotCommutative op
    interface IAbelianGroup<'t>

/// Category of groups with n structure-preserving morphisms.
type Groups<'ut, 'vt, 'n when 'ut : equality and 'vt: equality and 'n :> Number>(l:Group<'ut>, r:Group<'vt>, maps: Array<'n, Map<'ut, 'vt>>) = 
    inherit Category<'ut, 'vt, card.three, card.three, 'n>(l, r, maps) 

[<AutoOpen>]
module Group =
    /// Define a group over a set which has an additive operator and zero and negation. 
    let inline AdditiveGroup<'t when 't : equality and 't : (static member Zero:'t) and 't: (static member (+) :'t -> 't -> 't) and 't: (static member (~-) :'t -> 't)> 
        (set: ISet<'t>) =
        let zero = LanguagePrimitives.GenericZero<'t>
        let op = Binary(+).DestructureBinary
        { 
                new IAdditiveGroup<'t> with
                    member x.Set = set.Set
                    member x.Op = op
                    member x.Identity = zero
                    member x.Inverse = (~-)
                    member x.GetEnumerator(): Generic.IEnumerator<'t * 't * 't> = 
                        (let s = x.Set :> Generic.IEnumerable<'t> in s |> Seq.pairwise |> Seq.map (fun(a, b) -> (a, b, (op) a b))).GetEnumerator()
                    member x.GetEnumerator(): IEnumerator = (x :> Generic.IEnumerable<'t * 't * 't>).GetEnumerator () :> IEnumerator
        }

    /// Define a group over a set which has a multiplicative operator and one and division.
    let inline MultiplicativeGroup<'t when 't : equality and 't : (static member One:'t) and 't: (static member (*) :'t -> 't -> 't) and 't: (static member (/) :'t -> 't -> 't)>
        (set: ISet<'t>) =
        let one = LanguagePrimitives.GenericOne<'t>
        let inv = FSharpPlus.Math.Generic.(/) one
        let op = FSharpPlus.Math.Generic.(*)
        {
            new IMultiplicativeGroup<'t> with
                    member x.Set = set.Set
                    member x.Op = FSharpPlus.Math.Generic.(*)
                    member x.Identity = one
                    member x.Inverse = inv
                    member x.GetEnumerator(): Generic.IEnumerator<'t * 't * 't> = 
                        (let s = x.Set :> Generic.IEnumerable<'t> in s |> Seq.pairwise |> Seq.map (fun(a, b) -> (a, b, (op) a b))).GetEnumerator()
                    member x.GetEnumerator(): IEnumerator = (x :> Generic.IEnumerable<'t * 't * 't>).GetEnumerator () :> IEnumerator
        } 

    let Zero = FiniteAbelianGroup<N<1>, int>(Set.Zero, (*), 0, fun _ -> 0)