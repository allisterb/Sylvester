namespace Sylvester 

open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns

open Patterns
open Descriptions

type IConvergent<'t> = 
    inherit seq<'t>
    abstract Limit:'t

module Sequences =
    let desc = axiom_desc "Sequences"

    (* Definitions *)
    
    let lim (r:int) (s:seq<'t>) = formula<'t>

    
    let def_converges (epsilon:Expr<real>) (N:Expr<int>) (n:Expr<int>) (Li:Expr<real>) (d:Expr<seq<_>>) =      
            def PropCalculus.prop_calculus <@ %d :? IConvergent<_> = forall %epsilon (%epsilon > 0.) (exists %N  (%n > %N)  ((%Li - elem(%d).[%n]) < %epsilon)) @>

    let def_bounded (d:Expr<seq<_>>) =
            def PropCalculus.prop_calculus <@ %d :? IBounded<_> = sseq %d :? IBounded<_> @>

    (* Theorems *)
    let conv_implies_bound (s:Expr<seq<_>>) = proof PropCalculus.prop_calculus <@ %s :? IConvergent<_> ==> %s :? IConvergent<_> @> [] 