namespace Sylvester

open FSharp.Quotations

open SetTheory
open Sequences

module Topology =
    
    (* Theory *)

    type Topology() = inherit SetTheory<Vec<_>>()

    let topology = Topology()

    (* Predicates *)
  
    let closed_set = pred<Set<Vec<_>>>

    let open_set = pred<Set<Vec<_>>>

    let interior_point (x:Vec<_>) (S:Set<Vec<_>>) = pred<Set<Vec<_>>>

    (* Definitions *)

    let def_open_set (S:Expr<Set<Vec<_>>>) (x:Expr<Vec<_>>) (r:Expr<real>)= 
        def topology <@ open_set %S = forall %x (%x |?| %S) (exists %r (%r > 0.) ((open_ball %x %r) |<| %S)) @>

    let def_closed_set (S:Expr<Set<Vec<_>>>)= 
        def topology <@ closed_set %S = open_set (%S |/| R) @> //forall %x (%x |?| %S) (exists %r (%r > 0.) ((open_ball %x %r) |<| %S)) @>