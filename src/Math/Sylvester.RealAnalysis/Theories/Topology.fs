namespace Sylvester

open FSharp.Quotations

open Arithmetic
open SetTheory
open Vector
open Sequences
open Functions

type RegionExpr<'n when 'n :> Number> = Expr<Region<'n>>

module Topology =
    
    (* Theory *)

    type Topology<'n when 'n :> Number>() = inherit SetTheory<Vec<'n>>()

    let topology<'n when 'n :> Number> = Topology<'n>()

    (* Predicates *)
  
    let bounded = pred<Region<_>>

    let closed = pred<Region<_>>

    let ``open`` = pred<Region<_>>

    let interior_point (_:Region<_>) = pred<Vec<_>>

    let compact = pred<Region<_>>

    let continuous_at (a:Vec<'n>)= pred<(Vec<'n>->Vec<'m>)>

    let continuous_on (a:Region<'n>)= pred<(Vec<'n>->Vec<'m>)>

    let diffrentiable_at (a:Vec<'n>)= pred<(Vec<'n>->Vec<'m>)>

    let diffrentiable_on (a:Region<'n>)= pred<(Vec<'n>->Vec<'m>)>

    (* Functions *)
    
    let interior (S:Region<_>) = S |>| <@ interior_point S @>

    (* Definitions *)

    //let def_limit (epsilon:RealExpr) (N:Expr<int>) (n:Expr<int>) (Li:Expr<Vec<_>>) (a:Expr<int->Vec<_>>) =
    //    def sequences <@ lim_seq inf<int> (seq {(%a) %n}) = %Li = forall %epsilon (%epsilon > 0.) (exists %N  (%n > %N) ((euclid_dist %Li ((%a) %n)) < scalar %epsilon)) @>

    //let def_open (S:Expr<Set<Vec<_>>>) (x:Expr<Vec<_>>) (r:RealExpr)= 
    //    def topology <@ ``open`` %S = forall %x (%x |?| %S) (exists %r (%r > 0.) ((open_ball %x %r) |<| %S)) @>

    ///let def_closed (S:Expr<Set<Vec<_>>>)= 
    //    def topology <@ closed %S = ``open`` (%S |/| Field.R.Set) @>

    //let def_interior_point (S:RegionExpr<_>) (x:VecExpr<_>) (epsilon:RealExpr) =
    //    def topology <@ interior_point %S %x = (exists %epsilon (%epsilon > 0.) ((open_ball %x %epsilon) |<| %S)) @>

    let def_compact (S:RegionExpr<_>) (s:SeqExpr<Vec<_>>) (ss:SeqExpr<Vec<_>>)= 
        def topology <@ (compact %S) = forall %s (sseq %s |<| %S) (exists %ss (subsequence %ss %s) ((lim_seq inf<int> %ss) |?| %S)) @>

    //let def_lim (f:Expr<real->real>) (x:RealExpr) (a:RealExpr) (eps:RealExpr) (delta:RealExpr) (Li:RealExpr) = 
    //    def topology <@ lim <@ (%f)(%x) @> x a = %Li = forall %eps (%eps > 0.) (exists %delta (%x - %a < %delta) (((%f)(%x) - %Li) < %eps)) @>
    
    let def_continuous_at (S:RegionExpr<'n>) (f:Expr<Vec<'n>->Vec<'m>>) (a:VecExpr<'m>) (s:SeqExpr<Vec<'n>>) (x:VecExpr<'n>) =
        def topology <@ (dom %f = %S) |&| forall %s (sseq %s |<| %S |&| (lim_seq inf<int> %s = %x)) ((%f) %x = %a) @>

    let def_continuous_on (A:RegionExpr<'n>) (f:Expr<Vec<'n>->Vec<'m>>) (a:VecExpr<'n>)= 
        def topology <@ continuous_on %A %f = (forall %a (%a |?| %A) (continuous_at %a %f)) @>

    //let def_lim_n (f:Expr<Vec<'n>->Vec<'m>>) (x:VecExpr<'n>) (a:VecExpr<'m>) (eps:VecExpr<'n>) (delta:VecExpr<'n>) (Li:VecExpr<'n>) = 
    //    def functions <@ lim <@ (%f) (%x) @> x a = scalar %a = forall %eps (vnorm %eps > scalar 0.) (exists %delta (euclid_dist %x  %a < scalar %delta) (((%f)(%x) - %Li) < %eps)) @>
    

    //let def_
    (* Theorems *)
    let compact_implies_closed_bounded (S:RegionExpr<_>) =
        proof topology <@ compact %S ==> (closed %S |&| bounded %S) @> []