namespace Sylvester

open System
open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns

open Sylvester.Arithmetic
open Patterns
open Descriptions

open Vector

type VecExpr<'n when 'n :>Number > = Expr<Vec<'n>>

/// Theory of a Euclidean vector space over the field of real numbers
module EuclideanSpace =      
    let desc = axiom_desc "Euclidean Space"
    
    (* Axioms *)
    let eculidean_space_axioms<'n when 'n :> Number> = VectorSpace.vector_space_axioms<real>
