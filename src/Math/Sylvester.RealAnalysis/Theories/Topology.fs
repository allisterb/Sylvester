namespace Sylvester

open FSharp.Quotations

open Sylvester.Arithmetic

open SetTheory
open Vector
open Sequences

module Topology =
    
    (* Theory *)

    type Topology<'n when 'n :> Number>() = inherit SetTheory<Vec<'n>>()

    let topology<'n when 'n :> Number> = Topology<'n>()

    (* Predicates *)
  
    let closed_set = pred<Region<_>>

    let open_set = pred<Region<_>>

    let interior_point (_:Region<_>) = pred<Vec<_>>

    let compact = pred<Region<_>>

    let continuous_at (a:Vec<'n>)= pred<(Vec<'n>->Vec<'m>)>

    let continuous_on (a:Region<'n>)= pred<(Vec<'n>->Vec<'m>)>

    (* Functions *)
    
    [<Formula>]
    let interior (S:Region<_>) = S |>| interior_point S

    (* Definitions *)

    let def_limit (epsilon:real') (N:Expr<int>) (n:Expr<int>) (Li:Expr<Vec<_>>) (a:Expr<int->Vec<_>>) =
        def sequences <@ lim pos_inf (seq {(%a) %n}) = %Li = forall %epsilon (%epsilon > 0.) (exists %N  (%n > %N) ((vdist %Li ((%a) %n)) < scalar %epsilon)) @>

    let def_open_set (S:Expr<Set<Vec<_>>>) (x:Expr<Vec<_>>) (r:real')= 
        def topology <@ open_set %S = forall %x (%x |?| %S) (exists %r (%r > 0.) ((open_ball %x %r) |<| %S)) @>

    let def_closed_set (S:Expr<Set<Vec<_>>>)= 
        def topology <@ closed_set %S = open_set (%S |/| R) @>

    let def_interior_point (S:Region'<_>) (x:Vec'<_>) (epsilon:real') =
        def topology <@ interior_point %S %x = (exists %epsilon (%epsilon > 0.) ((open_ball %x %epsilon) |<| %S)) @>

    let def_compact (S:Region'<_>) (s:seq'<Vec<_>>) (ss:seq'<Vec<_>>)= 
        def topology <@ (compact %S) = forall %s (sseq %s |<| %S) (exists %ss (subsequence %ss %s) ((lim pos_inf %ss) |?| %S)) @>

    let def_continuous_at (S:Region'<'n>) (f:Expr<Vec<'n>->Vec<'m>>) (a:Vec'<'m>) (s:seq'<Vec<'n>>) (x:Vec'<'n>) =
        def topology <@ (dom %f = %S) |&| forall %s (sseq %s |<| %S |&| (lim pos_inf %s = %x)) ((%f) %x = %a) @>

    let def_continuous_on (A:Region'<'n>) (f:Expr<Vec<'n>->Vec<'m>>) (a:Vec'<'n>)= 
        def topology <@ continuous_on %A %f = (forall %a (%a |?| %A) (continuous_at %a %f)) @>

    (* Theorems *)
    let compact_implies_closed_bounded (S:Region'<_>) =
        proof topology <@ compact %S ==> (closed_set %S |&| bounded_set %S) @> []