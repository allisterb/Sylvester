module Functions

open Sylvester

let dom (f:'d->'r) = formula<Set<'d>>

let range (f:'d->'r) = formula<Set<'r>>

let relation (f:'t->'t) = formula<IRelation<'t>>

let relation' (f:'d->'r) = formula<IRelation<'d, 'r>>

let injection = pred<'d->'r>

let surjection = pred<'d->'r>

let bijection = pred<'d->'r>

let increasing = pred<'d->'r>

let increasing' = pred<'d->'r>

let decreasing = pred<'d->'r>

let decreasing' = pred<'d->'r>

let order = pred<'d->'r>