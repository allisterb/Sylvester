namespace Sylvester

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
    
/// Set of elements closed under some left-associative operation with identity and an inverse unary operation.
type Group<'t when 't: equality>(set:ISet<'t>, op:BinaryOp<'t>, id:'t, inv: UnaryOp<'t>) =
    inherit Monoid<'t>(set, op, id)
    member val Inverse = inv
    interface IGroup<'t> with member val Inverse = inv
    
type FiniteGroup<'order, 't when 'order :> Number and 't: equality>(set:FiniteSet<'order, 't>, op: BinaryOp<'t>, id:'t, inv: UnaryOp<'t>) =
    inherit Group<'t>(set, op, id, inv)
    member x.El0<'n when 'n :> card.one>() = (x, GroupElement<'order>(0))
    member x.El1<'n when 'n :> card.two>() = (x, GroupElement<'order>(0), GroupElement<'order>(1))

type AbelianGroup<'t when 't: equality>(set:ISet<'t>, op: BinaryOp<'t>, id:'t, inv: UnaryOp<'t>) =
    inherit Group<'t>(set, op, id, inv)
    do failIfNotCommutative op

type FiniteAbelianGroup<'order, 't when 'order :> Number and 't: equality>(set:FiniteSet<'order, 't>, op: BinaryOp<'t>, id:'t, inv: UnaryOp<'t>) =
    inherit AbelianGroup<'t>(set, op, id, inv)
    member x.El0<'n when 'n :> card.one>() = (x, GroupElement<'order>(0))
    member x.El1<'n when 'n :> card.two>() = (x, GroupElement<'order>(0), GroupElement<'order>(1))

/// Category of groups with n structure-preserving morphisms.
type Groups<'ut, 'vt, 'n when 'ut : equality and 'vt: equality and 'n :> Number>(l:Group<'ut>, r:Group<'vt>, maps: Array<'n, Map<'ut, 'vt>>) = 
    inherit Category<'ut, 'vt, card.one, card.one, 'n>(l, r, maps) 

[<AutoOpen>]
module Group =
    /// Define a group over a set which has a multiplicative operator and one and division.
    let inline MultiplicativeGroup<'t when 't : equality and 't : (static member One:'t) and 't: (static member (*) :'t -> 't -> 't) and 't: (static member (/) :'t -> 't -> 't)>
        (set: ISet<'t>) =
        let one = LanguagePrimitives.GenericOne<'t>
        AbelianGroup(set, FSharpPlus.Math.Generic.(*), one, FSharpPlus.Math.Generic.(/) one)

    /// Define a group over a set which has an additive operator and zero and negation. 
    let inline AdditiveGroup<'t when 't : equality and 't : (static member Zero:'t) and 't: (static member (+) :'t -> 't -> 't) and 't: (static member (~-) :'t -> 't)> 
        (set: ISet<'t>) =
        let id = LanguagePrimitives.GenericZero<'t>
        AbelianGroup(set, Binary(+).DestructureBinary, id, (~-))

    let Zero = FiniteAbelianGroup<N<1>, int>(Set.Zero, (*), 0, fun _ -> 0)