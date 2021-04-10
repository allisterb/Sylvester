namespace Sylvester 

open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns

open Patterns
open Descriptions

module Sequences =
    let desc = axiom_desc "Sequences"

    (* Definitions *)
    let converges = pred<seq<_>>
    
    let def_converges (epsilon:Expr<real>) (N:Expr<int>) (n:Expr<int>) (Li:Expr<real>) (d:Expr<seq<_>>) =      
            def PropCalculus.prop_calculus <@ converges %d = forall %epsilon (%epsilon > 0.) (exists %N  (%n > %N)  ((%Li - elem(%d).[%n]) < %epsilon)) @>

