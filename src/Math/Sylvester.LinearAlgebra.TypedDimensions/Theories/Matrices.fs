namespace Sylvester

open System
open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns

open Sylvester.Arithmetic
open Patterns
open Descriptions

type MatrixExpr<'dim0, 'dim1, 't when 'dim0 :> Number and 'dim1 :> Number and 't: equality and 't: comparison and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable> = 
    Expr<Matrix<'dim0, 'dim1, 't>>

/// Theory of linear maps between vector spaces
module MatrixAlgbra =      
    let desc = axiom_desc "Matrices"

    let matrix_axioms<'t when 't: equality and 't: comparison and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable> =
        let neg_one = neg_one_val(typeof<'t>)
        function
        | Assoc <@(=)@> (<@ (+) @> :Expr<Matrix<_, _, 't>->Matrix<_, _, 't>->Matrix<_, _, 't>>) x -> Some(desc x)
        | Identity <@(=)@> (<@ (+) @> : Expr<Matrix<_, _, 't>->Matrix<_, _, 't>->Matrix<_, _, 't>>) <@ Matrix<_, _, 't>.Zero @> x -> Some(desc x)
        //| Inverse <@(=)@> (<@ (+) @> :Expr<Matrix<_,_, 't>->Matrix<_,_, 't>->Matrix<_,_, 't>>) (expand_as <@ Matrix.msmul %%neg_one @>) <@ Matrix<_,_, 't>.Zero @> x
        | LeftCancelNonZero (<@ (+) @> :Expr<Matrix<_,_, 't>->Matrix<_,_, 't>->Matrix<_,_, 't>>) <@ Matrix<_,_, 't>.Zero @> x -> Some (desc x)
        | Exists(_, a::[], Bool true, (Equals(Add(Var _, Var a'), Value(v, t)))) when vequal a a' && t = typeof<Matrix<_,_, 't>> && (v :?> Matrix<_,_, 't>) = Matrix<_,_, 't>.Zero -> Some (desc (pattern_desc' "Additive Inverse"))
        | Commute' <@(=)@> (<@ (*) @> :Expr<Scalar<'t>->Matrix<_,_, 't>->Matrix<_,_, 't>>) x -> Some (desc x)
        | Distrib' <@(=)@> (<@ (*) @> :Expr<Scalar<'t>->Matrix<_,_, 't>->Matrix<_,_, 't>>) (<@ (+) @> :Expr<Matrix<_,_, 't>->Matrix<_,_, 't>->Matrix<_,_, 't>>) x  -> Some (desc x)

        | _ -> None
    
    (* Predicates *)
    
    let identity = pred<Matrix<_,_,_>>

    let symmetric = pred<Matrix<_,_,_>>

    let diagonal = pred<Matrix<_,_,_>>

    let diagonizable = pred<Matrix<_,_,_>>

    let invertible = pred<SquareMatrix<_,_>>

    let row_echelon_form = pred<Matrix<_,_,_>>

    let reduced_row_echelon_form = pred<Matrix<_,_,_>>

    let elementary = pred<Matrix<_,_,_>>

    let row_equivalent (_:Matrix<_,_,_>) = pred<Matrix<_,_,_>>

    let similar (_:Matrix<_,_,_>) = pred<Matrix<_,_,_>>

    (* Functions *)

    let null_space (_:Matrix<_,'dim1,_>) = formula<Set<Vector<'dim1,_>>>

    let row_space (_:Matrix<_,'dim1,_>) = formula<Set<Vector<'dim1,_>>>

    let column_space (_:Matrix<_,'dim1,_>) = formula<Set<Vector<'dim1,_>>>