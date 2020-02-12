namespace Sylvester

open Sylvester.Arithmetic

type Combination<'k, 't when 'k :> Number and 't : equality>(parent:Set<'t>, items:seq<'t>) =
    inherit FiniteSet<'k, 't>(items)
    member val Parent = parent
    member val Items = Seq.toList items

//type C<one, 't> = C of 't * 't