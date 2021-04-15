namespace Sylvester 

open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns

open Patterns
open Descriptions
open SetTheory

type IConvergent<'t> = 
    inherit seq<'t>
    abstract Limit:'t

module Sequences =
    let desc = axiom_desc "Sequences"

    (* Theory *)
    
    type Sequences<'t when 't: equality>() = inherit SetTheory<'t>()
    
    let sequences<'t when 't: equality> = Sequences<'t>()

    (* Definitions *)
    
    let lim (r:int) (s:seq<'t>) = formula<'t>

    let subsequence (s:seq<'t>) = pred<seq<_>>

    let converges = pred<seq<_>>

    let bounded_above = pred<seq<_>>
    
    let bounded_below = pred<seq<_>>

    let bounded = pred<seq<_>>

    let increasing = pred<seq<_>>

    let decreasing = pred<seq<_>>

    let monotonic = pred<seq<_>>

    let def_seq (s:Expr<seq<_>>) (n:Expr<int>) (a:Expr<Term<_>>) (f:Expr<int->_>)=
        def sequences <@ %s = seq {(%a).[%n]} @>

    let def_limit (epsilon:Expr<real>) (N:Expr<int>) (n:Expr<int>) (Li:Expr<real>) (a:Expr<fn<int, real>>) =
        def sequences <@ lim pos_inf (seq {(%a).[%n]}) = %Li = forall %epsilon (%epsilon > 0.) (exists %N  (%n > %N)  ((%Li - (%a).[%n]) < %epsilon)) @>

    let def_subsequence (n:Expr<int>) (a:Expr<Term<real>>) (f:Expr<int->int>) =
        def sequences <@ subsequence (seq {(%a).[%n]}) (seq {(%a).[(%f) %n]}) = Function.increasing %f @>

    let def_converges (epsilon:Expr<real>) (N:Expr<int>) (n:Expr<int>) (Li:Expr<real>) (a:Expr<Term<real>>) =      
            def sequences <@ converges (seq {(%a).[%n]})  = exists' %Li (lim pos_inf (seq {(%a).[%n]}) = %Li) @> 
    
    let def_bounded_above (d:Expr<seq<_>>) =
            def sequences <@ bounded %d = sseq %d :? IBoundedAbove<_> @>

    let def_bounded (d:Expr<seq<_>>) =
            def sequences <@ bounded %d = sseq %d :? IBounded<_> @>

    let def_increasing (n:Expr<int>) (a:Expr<Term<real>>) =
        def sequences <@ increasing (seq {(%a).[%n]}) = forall' %n ((%a).[(%n)+1] > (%a).[(%n)]) @>

    let def_decreasing (n:Expr<int>) (a:Expr<Term<real>>) =
        def sequences <@ decreasing (seq {(%a).[%n]}) = forall' %n ((%a).[(%n)+1] < (%a).[(%n)]) @>

    let def_monotonic (s:Expr<seq<_>>) =
        def sequences <@ monotonic %s = increasing %s ||| decreasing %s @>
    (* Theorems *)

    let conv_implies_bound (s:Expr<seq<_>>) = proof sequences <@ converges %s  ==> bounded %s  @> [] 

    let subseq_limit (s:Expr<seq<_>>) (ss:Expr<seq<_>>) (n:Expr<int>) (Li:Expr<real>) =
        proof sequences <@ lim %n %s = %Li |&| subsequence %s %ss ==> (lim %n %ss = %Li) @>
        
    let lim_algebra_const_eq (a:Expr<Term<real>>) (n:Expr<int>) (Li:Expr<real>) (C:Expr<real>) = proof sequences <@ lim pos_inf (seq {(%a).[%n]}) = %Li ==> (lim pos_inf (seq {%C * (%a).[%n]}) = %C * %Li) @> []