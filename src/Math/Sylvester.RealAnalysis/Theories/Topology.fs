namespace Sylvester

open Sylvester.Arithmetic
open Sequences

module Topology =
    
    (* Predicates *)
    let sup = pred<OrderedSet<'t>>
    let inf = pred<OrderedSet<'t>>
    let fo = ()    
