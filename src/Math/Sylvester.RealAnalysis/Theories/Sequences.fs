namespace Sylvester 

open FSharp.Quotations

open Descriptions
open SetTheory
open Series
open Vector

module Sequences =
    let desc = axiom_desc "Sequences"

    (* Theory *)
    
    type Sequences<'t when 't: equality>() = inherit SetTheory<'t>()
    
    let sequences<'t when 't: equality> = Sequences<'t>()

    (* Functions *)

    let lim_seq (r:int) (s:seq<'t>) = formula<'t>
    
    (* Predicates *)
    
    let subsequence (s:seq<'t>) = pred<seq<_>>

    let converges = pred<seq<_>>

    let diverges = pred<seq<_>>

    let bounded_above = pred<seq<_>>
    
    let bounded_below = pred<seq<_>>

    let bounded = pred<seq<_>>

    let increasing = pred<seq<_>>

    let decreasing = pred<seq<_>>

    let monotonic = pred<seq<_>>

    let null_sequence = pred<seq<_>>

    let cauchy = pred<seq<_>>

    (* Definitions *)

    let def_seq_n (f:Expr<int->_>) (n:Expr<int>) =
        def sequences <@ infinite_seq %f = seq {(%f) %n} @>

    let def_series_n (f:Expr<int->real>) (n:Expr<int>) =
        def sequences <@ infinite_series %f = seq { partial_sum %n (seq {(%f) %n}) } @>

    let def_lim_seqit (epsilon:Expr<real>) (N:Expr<int>) (n:Expr<int>) (Li:Expr<real>) (a:Expr<int->real>) =
        def sequences <@ lim_seq inf<int> (seq {(%a) %n}) = %Li = forall %epsilon (%epsilon > 0.) (exists %N  (%n > %N)  (abs(%Li - (%a) %n) < %epsilon)) @>
    
    let def_subsequence (n:Expr<int>) (a:Expr<int->_>) (f:Expr<int->int>) =
        def sequences <@ subsequence (seq {(%a) %n}) (seq {((%a) << (%f)) %n}) = Function.increasing %f @>

    let def_converges (a:Expr<int->_>) (Li:Expr<_>)  =      
            def sequences <@ converges (infinite_seq %a) = (lim_seq inf<int> (infinite_seq %a) = %Li) @> 
    
    let def_bounded_above (d:Expr<seq<_>>) =
            def sequences <@ bounded %d = SetTheory.bounded_above (sseq %d) @>

    let def_bounded (d:Expr<seq<_>>) =
            def sequences <@ bounded %d = SetTheory.bounded (sseq %d)  @>

    let def_increasing (n:Expr<int>) (a:Expr<int->_>) =
        def sequences <@ increasing (seq {(%a) %n}) = forall' %n ((%a)((%n)+1) > (%a) %n) @>

    let def_decreasing (n:Expr<int>) (a:Expr<int->_>) =
        def sequences <@ decreasing (seq {(%a) %n}) = forall' %n ((%a) ((%n)+1) < (%a) %n) @>

    let def_monotonic (s:Expr<seq<_>>) =
        def sequences <@ monotonic %s = (increasing %s ||| decreasing %s) @>

    let def_null_sequence(a:Expr<int->real>) (epsilon:Expr<real>) (k: Expr<int>) (K:Expr<int>) =
        def sequences <@ null_sequence (seq {(%a) %k}) = forall %epsilon (%epsilon > 0.) (exists %K (%K > 0) (forall' %K  (abs((%a) %k) < %epsilon))) @>
    
    (* Theorems *)

    let seq_conv_implies_bound (s:Expr<seq<_>>) = proof sequences <@ converges %s  ==> bounded %s  @> [] 

    let seq_divg_implies_notbound (s:Expr<seq<_>>) = proof sequences <@ diverges %s  ==> not (bounded %s)  @> []

    let seq_bounded_mono_implies_converg (s:SeqExpr<_>)= proof sequences <@ bounded %s |&|monotonic %s ==> converges %s @> []

    let seq_exists_mono_seq (s:SeqExpr<_>) (ss:SeqExpr<_>) = proof sequences <@ forall' %s (exists %ss (subsequence %s %ss) (monotonic %ss)) @> []

    let seq_bolzano_weierstrass (s:SeqExpr<real>) (ss:SeqExpr<real>) = proof sequences <@ forall %s (bounded %s) (exists %ss (subsequence %s %ss) (converges %ss)) @> []

    let lim_seqit_subseq (s:Expr<seq<_>>) (ss:Expr<seq<_>>) (n:Expr<int>) (Li:Expr<real>) =
        proof sequences <@ lim_seq %n %s = %Li |&| subsequence %s %ss ==> (lim_seq %n %ss = %Li) @>
        
    let lim_seq_algebra_const_eq (a:Expr<int->real>) (n:Expr<int>) (C:Expr<real>) = 
        proof sequences <@ lim_seq inf<int> (seq {%C * (%a) %n}) = %C * lim_seq inf<int> (seq {(%a) %n}) @> []

    let lim_seq_algebra_abs_eq (a:Expr<int->real>) (n:Expr<int>) = 
        proof sequences <@ lim_seq inf<int> (seq {abs((%a) %n)}) = abs(lim_seq inf<int> (seq {(%a) %n})) @> []

    let lim_seq_algebra_add_eq (a:Expr<int->real>) (b:Expr<int->real>) (n:Expr<int>)  = 
        proof sequences <@ lim_seq inf<int> (seq {(%a) %n + (%b) %n}) = lim_seq inf<int> (seq {(%a) %n}) + lim_seq inf<int> (seq {(%b) %n}) @> []

    let lim_seq_algebra_mul_eq (a:Expr<int->real>) (b:Expr<int->real>) (n:Expr<int>)  = 
        proof sequences <@ lim_seq inf<int> (seq {(%a) %n * (%b) %n}) = lim_seq inf<int> (seq {(%a) %n}) * lim_seq inf<int> (seq {(%b) %n}) @> []
    
    let lim_seq_algebra_div_eq (a:Expr<int->real>) (n:Expr<int>) (Li:Expr<real>) = 
        proof sequences <@ forall' %n ((%a) %n <> 0.) && %Li <> 0. ==> (lim_seq inf<int> (seq {1./(%a) %n}) = 1. / %Li) @> []

    let lim_seq_algebra_pow_eq (a:Expr<int->real>) (n:Expr<int>) (k:Expr<real>) = 
        proof sequences <@ lim_seq inf<int> (seq {((%a) %n) ** %k}) = lim_seq inf<int> (seq {(%a) %n}) ** %k @> []

    let lim_seq_algebra_gt0_eq (a:Expr<int->real>) (n:Expr<int>) = 
        proof sequences <@ forall' %n ((%a) %n > 0.) ==> (lim_seq inf<int> (seq {(%a) %n}) > 0.) @> []

    let lim_seq_geom_series (a:Expr<real>) (r:Expr<real>)  = 
        proof sequences <@ lim_seq inf<int> (geometric_series %a %r)  = (%a / real 1 - %r) @> []

    let series_conv_implies_lim_seqit_zero (a:Expr<int->real>)  =
        proof sequences <@ converges (infinite_series %a) ==> (lim_seq inf<int> (infinite_seq %a) = 0.) @>

   