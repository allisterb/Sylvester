namespace Sylvester

open Sylvester.Arithmetic

type KnownCombination<'k, 't when 'k :> Number and 't : equality>(parent:Set<'t>, items:seq<'t>) =
    inherit KnownFiniteSet<'k, 't>(items)
    member val Parent = parent
    member val Items = Seq.toList items

//type C<one, 't> = C of 't * 't