namespace Sylvester

open System.Collections
open FSharp.Quotations
open Sylvester
open Arithmetic
open N10
open Sylvester.Collections

[<RequireQualifiedAccess>]
module internal Mod = 
    // Based on: https://paulor.land/writing/algebra-beyond-numbers/index.html#modular-arithmetic
    let modulo n x = (x % n + n) % n
    
    let (+) n x y = modulo n (x + y)
    let (-) n x y = modulo n (x - y)
    let (*) n x y = modulo n (x * y)
    let (/) n x y = x % y

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

type IAbelianGroup<'n, 't when 'n :> Number and 't : equality> =
    inherit IAbelianGroup<'t>
    abstract Order: 'n

type IAdditiveGroup<'t when 't: equality> = inherit IAbelianGroup<'t>

type IMultiplicativeGroup<'t when 't: equality> = inherit IAbelianGroup<'t>

type ISubGroup<'t when 't: equality> = 
    inherit IGroup<'t>
    abstract Parent:IGroup<'t> 

type ISubGroup<'order, 't when 'order :> Number and 't : equality> = inherit ISubGroup<'t>

/// Set of elements closed under some left-associative operation with identity and an inverse unary operation.
type Group<'t when 't: equality>(set:ISet<'t>, op:BinaryOp<'t>, ident:'t, inv: UnaryOp<'t>) =
    inherit Struct<'t, ``3``>(set, arrayOf3 (Binary(op)) (Nullary(ident)) (Unary(inv)))    
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
    member x.Subgroup(p:Expr<'t -> bool>) = 
        { 
            new ISubGroup<'t> with 
                member a.Parent = x :> IGroup<'t>
                member a.Set = x.Set.Subset p
                member a.Equals b = x.Set.Equals b
                member a.Op = x.Op
                member a.Identity = x.Identity
                member a.Inverse  = x.Inverse
                member x.GetEnumerator(): Generic.IEnumerator<'t * 't * 't> = 
                     (let s = x.Set :> Generic.IEnumerable<'t> in s |> Seq.pairwise |> Seq.map (fun(a, b) -> (a, b, (op) a b))).GetEnumerator()
                 member x.GetEnumerator(): IEnumerator = (x :> Generic.IEnumerable<'t * 't * 't>).GetEnumerator () :> IEnumerator
 
        } 
    static member (|>|) (l:Group<'t> , r:Expr<'t -> bool> when 't : equality) = l.Subgroup r
 
type AbelianGroup<'t when 't: equality>(set:ISet<'t>, op: BinaryOp<'t>, id:'t, inv: UnaryOp<'t>) =
    inherit Group<'t>(set, op, id, inv)
    do op |> fail_if_not_commutative
    interface IAbelianGroup<'t>

/// Finite group of known order.
type Group<'order, 't when 'order :> Number and 't: equality>(set:FiniteSet<'order, 't>, op: BinaryOp<'t>, ident:'t, inv: UnaryOp<'t>) =
    inherit Group<'t>(set, op, ident, inv)
    member x.El0<'n when 'n :> ``1``>() = (x, GroupElement<'order>(0))
    member x.El1<'n when 'n :> ``2``>() = (x, GroupElement<'order>(0), GroupElement<'order>(1))

/// Finite abelian group of known order.
type AbelianGroup<'order, 't when 'order :> Number and 't: equality>(set:FiniteSet<'order, 't>, op: BinaryOp<'t>, ident:'t, inv: UnaryOp<'t>) =
    inherit Group<'order, 't>(set, op, ident, inv)
    do op |> fail_if_not_commutative
    interface IAbelianGroup<'t>

/// Category of groups with n structure-preserving morphisms.
type Groups<'ut, 'vt, 'n when 'ut : equality and 'vt: equality and 'n :> Number>(l:Group<'ut>, r:Group<'vt>, maps: Array<'n, Map<'ut, 'vt>>) = 
    inherit Category<'ut, 'vt, ``3``, ``3``, 'n>(l, r, maps) 

[<AutoOpen>]
module Group =
    /// Define a group over a set which has an additive operator and zero and negation. 
    let inline additive_group<'t when 't : equality and 't : (static member Zero:'t) and 't: (static member (+) :'t -> 't -> 't) and 't: (static member (~-) :'t -> 't)> 
        (set: ISet<'t>) =
        let zero = LanguagePrimitives.GenericZero<'t>
        let op = Binary(+).DestructureBinary
        { 
            new IAdditiveGroup<'t> with
                member x.Set = set.Set
                member x.Equals y = x.Set.Equals y
                member x.Op = op
                member x.Identity = zero
                member x.Inverse = (~-)
                member x.GetEnumerator(): Generic.IEnumerator<'t * 't * 't> = 
                    (let s = x.Set :> Generic.IEnumerable<'t> in s |> Seq.pairwise |> Seq.map (fun(a, b) -> (a, b, (op) a b))).GetEnumerator()
                member x.GetEnumerator(): IEnumerator = (x :> Generic.IEnumerable<'t * 't * 't>).GetEnumerator () :> IEnumerator
        }

    /// Define a group over a set which has a multiplicative operator and one and division.
    let inline multiplicative_group<'t when 't : equality and 't : (static member One:'t) and 't: (static member (*) :'t -> 't -> 't) and 't: (static member (/) :'t -> 't -> 't)>
        (set: ISet<'t>) =
        let one = LanguagePrimitives.GenericOne<'t>
        let inv = FSharpPlus.Math.Generic.(/) one
        let op = FSharpPlus.Math.Generic.(*)
        {
            new IMultiplicativeGroup<'t> with
                    member x.Set = set.Set
                    member x.Equals y = x.Set.Equals y
                    member x.Op = FSharpPlus.Math.Generic.(*)
                    member x.Identity = one
                    member x.Inverse = inv
                    member x.GetEnumerator(): Generic.IEnumerator<'t * 't * 't> = 
                        (let s = x.Set :> Generic.IEnumerable<'t> in s |> Seq.pairwise |> Seq.map (fun(a, b) -> (a, b, (op) a b))).GetEnumerator()
                    member x.GetEnumerator(): IEnumerator = (x :> Generic.IEnumerable<'t * 't * 't>).GetEnumerator () :> IEnumerator
        } 

    let Zero = AbelianGroup<dim<1>, int>(Set.Zero, (*), 0, fun _ -> 0)