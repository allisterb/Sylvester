namespace Sylvester

open FSharp.Quotations

open Sylvester.Arithmetic


open Vector
open Sequences
open Topology

type RealExpr<'n when 'n :> Number> = Expr<R<'n>>

type Functions<'n when 'n:> Number>() = inherit Topology<'n>()

module Functions =
    
    (* Theory *)
    let functions<'n when 'n :> Number> = Functions<'n>()

    (* Predicates *)
    
    let continuous_at (a:Vec<'n>)= pred<(Vec<'n>->Vec<'m>)>

    let continuous_on (a:Region<'n>)= pred<(Vec<'n>->Vec<'m>)>

    let diffrentiable_at (a:Vec<'n>)= pred<(Vec<'n>->Vec<'m>)>

    let diffrentiable_on (a:Region<'n>)= pred<(Vec<'n>->Vec<'m>)>

    (* Definitions *)
    
    let def_lim (f:Expr<real->real>) (x:RealExpr) (a:RealExpr) (eps:RealExpr) (delta:RealExpr) (Li:RealExpr) = 
        def functions <@ lim <@ (%f)(%x) @> x a = scalar %Li = forall %eps (%eps > 0.) (exists %delta (%x - %a < %delta) (((%f)(%x) - %Li) < %eps)) @>
    
    let def_continuous_at (S:RegionExpr<'n>) (f:Expr<Vec<'n>->Vec<'m>>) (a:VecExpr<'m>) (s:SeqExpr<Vec<'n>>) (x:VecExpr<'n>) =
        def topology <@ (dom %f = %S) |&| forall %s (sseq %s |<| %S |&| (lim_seq pos_inf %s = %x)) ((%f) %x = %a) @>

    let def_continuous_on (A:RegionExpr<'n>) (f:Expr<Vec<'n>->Vec<'m>>) (a:VecExpr<'n>)= 
        def topology <@ continuous_on %A %f = (forall %a (%a |?| %A) (continuous_at %a %f)) @>

    //let def_lim_n (f:Expr<Vec<'n>->Vec<'m>>) (x:VecExpr<'n>) (a:VecExpr<'m>) (eps:VecExpr<'n>) (delta:VecExpr<'n>) (Li:VecExpr<'n>) = 
    //    def functions <@ lim <@ (%f) (%x) @> x a = scalar %a = forall %eps (vnorm %eps > scalar 0.) (exists %delta (euclid_dist %x  %a < scalar %delta) (((%f)(%x) - %Li) < %eps)) @>
    
