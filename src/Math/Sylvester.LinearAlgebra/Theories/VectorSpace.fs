namespace Sylvester

open System
open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns

open Sylvester.Arithmetic
open Patterns
open Descriptions

type VectorSet<'dim0, 't when 'dim0 :> Number and 't: equality and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable> = 
    Expr<Set<Vector<'dim0, 't>>>

/// Theory of vector spaces and subspaces.
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

    (* Predicates *)

    let linearly_independent = pred<VectorSet<_,_>>
