﻿namespace Sylvester

open System.Collections

open Sylvester
open Arithmetic
open N10
open Sylvester.Collections

/// Set of elements closed under some left-associative operation with identity.
type IMonoid<'t when 't: equality> = 
    inherit ISemigroup<'t>
    inherit IIdentity<'t>

type IAdditiveMonoid<'t when 't : equality> = IMonoid<'t>

type IMultiplicativeMonoid<'t when 't : equality> = IMonoid<'t>

/// Set of elements closed under some left-associative operation with identity element.
type Monoid<'t when 't: equality>(set:ISet<'t>, op:BinaryOp<'t>, id: NullaryOp<'t>) =
    inherit Struct<'t, ``2``>(set, arrayOf2 (Binary(op)) (Nullary(id)))
    member val Op = op
    member val Identity = id
    interface IMonoid<'t> with 
        member val Op = op
        member val Identity = id
        member x.GetEnumerator(): Generic.IEnumerator<'t * 't * 't> = 
            (let s = x.Set :> Generic.IEnumerable<'t> in s |> Seq.pairwise |> Seq.map (fun(a, b) -> (a, b, (op) a b))).GetEnumerator()
        member x.GetEnumerator(): IEnumerator = (x :> Generic.IEnumerable<'t * 't * 't>).GetEnumerator () :> IEnumerator

/// Monoid with commutative operators.
type CommutativeMonoid<'t when 't: equality>(set:ISet<'t>, op:BinaryOp<'t>, id:'t) =
    inherit Monoid<'t>(set, op, id)
    do op |> fail_if_not_commutative

/// Category of monoids with n structure-preserving morphisms.
type Monoids<'ut, 'vt, 'n when 'ut : equality and 'vt: equality and 'n :> Number>(l:Monoid<'ut>, r:Monoid<'vt>, maps: Array<'n, 'ut->'vt>) = 
    inherit Category<'ut, 'vt, ``2``, ``2``,'n>(l, r, maps)

[<AutoOpen>]
module Monoid =
    /// Define a monoid over a set which has an additive operator and zero. 
    let inline additive_monoid<'t when 't : equality and 't : (static member Zero:'t) and 't: (static member (+) :'t -> 't -> 't)> 
        (set: ISet<'t>) =
        let id = LanguagePrimitives.GenericZero<'t>
        let op = Binary(+).DestructureBinary
        {
            new CommutativeMonoid<'t>(set, op, id)
                interface IAdditiveMonoid<'t> with
                    member x.Op = op
                    member x.Identity = id
                    member x.GetEnumerator(): Generic.IEnumerator<'t * 't * 't> = 
                        (let s = x.Set :> Generic.IEnumerable<'t> in s |> Seq.pairwise |> Seq.map (fun(a, b) -> (a, b, (op) a b))).GetEnumerator()
                    member x.GetEnumerator(): IEnumerator = (x :> Generic.IEnumerable<'t * 't * 't>).GetEnumerator () :> IEnumerator

        }
    /// Define a monoid over a set which has a multiplicative operator and one. 
    let inline multiplicative_monoid<'t when 't : equality and 't : (static member One:'t) and 't: (static member (*) :'t -> 't -> 't)> 
        (set: ISet<'t>) =
        let one = LanguagePrimitives.GenericOne<'t>
        let op = FSharpPlus.Math.Generic.(*)
        {
            new CommutativeMonoid<'t>(set, op, one)
                interface IAdditiveMonoid<'t> with
                    member x.Op = op
                    member x.Identity = one
                    member x.GetEnumerator(): Generic.IEnumerator<'t * 't * 't> = 
                        (let s = x.Set :> Generic.IEnumerable<'t> in s |> Seq.pairwise |> Seq.map (fun(a, b) -> (a, b, (op) a b))).GetEnumerator()
                    member x.GetEnumerator(): IEnumerator = (x :> Generic.IEnumerable<'t * 't * 't>).GetEnumerator () :> IEnumerator

        }

    let Zero = CommutativeMonoid(Set.Zero, (*), 0)