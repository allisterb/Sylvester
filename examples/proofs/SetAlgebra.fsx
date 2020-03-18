#load "..\\math\\MathInclude.fsx"

open Sylvester
open Sylph
open SetAlgebraTheory


let x,y, z = var3<Set<obj>>
//let t_3_1 = <@(x |+| y = U) |&| (x |*| y = Set.Empty) ==> (y = x)@> |> theorem set_algebra []

//(expand <@(y |+| y) = y@>) |> body 
//let l1 = ident_axiom set_algebra <@x |*| U = (x) @>
<@ U  @> 
