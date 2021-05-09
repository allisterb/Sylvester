namespace Sylvester

open System
open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns

open Sylvester.Arithmetic
open Patterns
open Descriptions

open SetTheory

type VectorSetExpr<'dim0, 't when 'dim0 :> Number and 't: equality and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable> = 
    Expr<Set<Vector<'dim0, 't>>>

/// Theory of vector spaces, inner product spaces, and subspaces.
module VectorSpace =      
    let desc = axiom_desc "Vector Space"
    
    (* Axioms *)
    
    let vector_space_axioms<'t when 't: equality and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable> =
        let neg_one = neg_one_val(typeof<'t>)
        let one = one_val(typeof<'t>) |> expand'' |> Scalar<'t>
        function                            
        | Assoc <@(=)@> (<@ (+) @> :Expr<Vector<_,'t>->Vector<_,'t>->Vector<_,'t>>) x
        | Commute <@(=)@> (<@ (+) @> :Expr<Vector<_,'t>->Vector<_,'t>->Vector<_,'t>>) x
        | Identity <@(=)@> (<@ (+) @> :Expr<Vector<_,'t>->Vector<_,'t>->Vector<_,'t>>) <@ Vector<_,'t>.Zero @> x 
        | Inverse <@(=)@> (<@ (+) @> :Expr<Vector<_, 't>->Vector<_, 't>->Vector<_, 't>>) (expand'' <@ Vector.smul %%neg_one @>) <@ Vector<_, 't>.Zero @> x
        | Commute' <@(=)@> (<@ (*) @> :Expr<Scalar<_>->Vector<_, 't>->Vector<_, 't>>) x -> Some (desc x)
        | Distrib' <@(=)@> (<@ (*) @> :Expr<Scalar<_>->Vector<_, 't>->Vector<_, 't>>) (<@ (+) @> :Expr<Vector<_, 't>->Vector<_, 't>->Vector<_, 't>>) x  -> Some (desc x)
        | Distrib'' <@(=)@> (<@ (*) @> :Expr<Scalar<_>->Vector<_, 't>->Vector<_, 't>>) (<@ (+) @> :Expr<Vector<_, 't>->Vector<_, 't>->Vector<_, 't>>) x  -> Some (desc x)
        | Assoc' <@(=)@> (<@ (*) @> :Expr<Scalar<'t>->Vector<_,'t>->Vector<_,'t>>) x -> Some (desc x)
        | Identity <@(=)@> (<@ (*) @> :Expr<Scalar<'t>->Scalar<'t>->Scalar<'t>>) (expand'' <@ one @>) x -> Some (desc x)
        | _ -> None

    let inner_product_space_axioms<'t when 't: equality and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable> =
        let neg_one = neg_one_val(typeof<'t>)
        let one = one_val(typeof<'t>) |> expand'' |> Scalar<'t>
        function                            
        | Commute <@(=)@> (<@ (*) @> :Expr<Vector<_,'t>->Vector<_,'t>->Scalar<'t>>) x -> Some(desc x)
        | _ -> None

    type VectorSpace<'n, 't when 'n :> Number and 't: equality and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable>() = inherit SetTheory<Vector<'n,'t>>(vector_space_axioms)

    let vector_space<'n,'t> = VectorSpace<_,_>()

    (* Functions *)

    let linear_relation (_:Set<Scalar<'t>>) (_:Set<Vector<_,'t>>) = formula<Vector<_,'t>>

    let span (_:Set<Vector<'t>>) = formula<Set<Vector<_,'t>>>

    (* Predicates *)

    let linear_combination (b:Set<Vector<_,_>>) = pred<Vector<_,_>>

    let linearly_independent = pred<Set<Vector<_,_>>>

    let basis (_:VectorSpace<_,_,'v>) = pred<Set<'v>>

    let subspace (_:VectorSpace<_,_,'v>) = pred<Set<'v>>