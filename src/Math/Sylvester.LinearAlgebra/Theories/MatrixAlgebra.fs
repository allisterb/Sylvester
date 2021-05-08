namespace Sylvester

open System
open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns

open Sylvester.Arithmetic
open Patterns
open Descriptions

type MatrixExpr<'dim0, 'dim1, 't when 'dim0 :> Number and 'dim1 :> Number and 't: equality and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable> = 
    Expr<Matrix<'dim0, 'dim1, 't>>

/// Theory of operations in a vector space over a field of scalars
module MatrixAlgbra =      
    let desc = axiom_desc "Matrix Algebra"

    let matrix_axioms<'t when 't: equality and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable> =
        let neg_one = neg_one_val(typeof<'t>)
        function
        | Assoc <@(=)@> (<@ (+) @> :Expr<Matrix<_, _, 't>->Matrix<_, _, 't>->Matrix<_, _, 't>>) x -> Some(desc x)
        | Identity <@(=)@> (<@ (+) @> : Expr<Matrix<_, _, 't>->Matrix<_, _, 't>->Matrix<_, _, 't>>) <@ Matrix<_, _, 't>.Zero @> x -> Some(desc x)
        | Inverse <@(=)@> (<@ (+) @> :Expr<Matrix<_,_, 't>->Matrix<_,_, 't>->Matrix<_,_, 't>>) (expand'' <@ Matrix.smul %%neg_one @>) <@ Matrix<_,_, 't>.Zero @> x
        | LeftCancelNonZero (<@ (+) @> :Expr<Matrix<_,_, 't>->Matrix<_,_, 't>->Matrix<_,_, 't>>) <@ Matrix<_,_, 't>.Zero @> x -> Some (desc x)
        | Exists(_, a::[], Bool true, (Equals(Add(Var _, Var a'), Value(v, t)))) when vequal a a' && t = typeof<Matrix<_,_, 't>> && (v :?> Matrix<_,_, 't>) = Matrix<_,_, 't>.Zero -> Some (desc (pattern_desc' "Additive Inverse"))
        | Commute' <@(=)@> (<@ (*) @> :Expr<Scalar<_>->Matrix<_,_, 't>->Matrix<_,_, 't>>) x -> Some (desc x)
        | Distrib' <@(=)@> (<@ (*) @> :Expr<Scalar<_>->Matrix<_,_, 't>->Matrix<_,_, 't>>) (<@ (+) @> :Expr<Matrix<_,_, 't>->Matrix<_,_, 't>->Matrix<_,_, 't>>) x  -> Some (desc x)

        | _ -> None
    
