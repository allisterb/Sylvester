namespace Sylvester

open System
open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns

open Sylvester.Arithmetic
open Patterns
open Descriptions

open Vector

type VectorExpr<'dim0, 't when 'dim0 :> Number and 't: equality and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable> = 
    Expr<Vector<'dim0, 't>>

type MatrixExpr<'dim0, 'dim1, 't when 'dim0 :> Number and 'dim1 :> Number and 't: equality and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable> = 
    Expr<Matrix<'dim0, 'dim1, 't>>

/// Theory of operations in a vector space over a field of scalars
module VectorSpace =      
    let desc = axiom_desc "Linear Algebra"
    
    (* Axioms *)
    
    let vector_axioms<'t when 't: equality and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable> =
        let neg_one = neg_one_val(typeof<'t>)
        function                            
        | Assoc <@(=)@> (<@ (+) @> :Expr<Vector<_,'t>->Vector<_,'t>->Vector<_,'t>>) x
        | Commute <@(=)@> (<@ (+) @> :Expr<Vector<_, 't>->Vector<_, 't>->Vector<_, 't>>) x
        | Identity <@(=)@> (<@ (+) @> :Expr<Vector<_, 't>->Vector<_, 't>->Vector<_, 't>>) <@ Vector<_, 't>.Zero @> x 
        | Inverse <@(=)@> (<@ (+) @> :Expr<Vector<_, 't>->Vector<_, 't>->Vector<_, 't>>) (expand'' <@ vsmul %%neg_one @>) <@ Vector<_, 't>.Zero @> x
        | LeftCancelNonZero (<@ (+) @> :Expr<Vector<_, 't>->Vector<_, 't>->Vector<_, 't>>) <@ Vector<_, 't>.Zero @> x -> Some (desc x)
        | Exists(_, a::[], Bool true, (Equals(Add(Var _, Var a'), Value(v, t)))) when vequal a a' && t = typeof<Vector<_, 't>> && (v :?> Vector<_, 't>) = Vector<_, 't>.Zero -> Some (desc (pattern_desc' "Additive Inverse"))
        | Commute' <@(=)@> (<@ (*) @> :Expr<Scalar<_>->Vector<_, 't>->Vector<_, 't>>) x -> Some (desc x)
        | Distrib' <@(=)@> (<@ (*) @> :Expr<Scalar<_>->Vector<_, 't>->Vector<_, 't>>) (<@ (+) @> :Expr<Vector<_, 't>->Vector<_, 't>->Vector<_, 't>>) x  -> Some (desc x)
        | _ -> None
    
    let matrix_axioms<'dim0, 'dim1, 't when 'dim0 :> Number and 'dim1 :> Number and 't: equality and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable> =
        let one = one_val(typeof<'t>)
        function
        | Assoc <@(=)@> (<@ (+) @> :Expr<Matrix<'dim0, 'dim1, 't>->Matrix<'dim0, 'dim1, 't>->Matrix<'dim0, 'dim1, 't>>) x -> Some(desc x)
        | Commute <@(=)@> (<@ (+) @> :Expr<Matrix<'dim0, 'dim1, 't>->Matrix<'dim0, 'dim1, 't>->Matrix<'dim0, 'dim1, 't>>) x -> Some(desc x)
        | Identity <@(=)@> (<@ (+) @> : Expr<Matrix<'dim0, 'dim1, 't>->Matrix<'dim0, 'dim1, 't>->Matrix<'dim0, 'dim1, 't>>) <@ Matrix<'dim0, 'dim1, 't>.Zero @> x -> Some(desc x)
        | _ -> None