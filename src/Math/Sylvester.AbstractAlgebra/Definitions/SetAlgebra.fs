namespace Sylvester

type SetAlgebra<'t when 't: equality>(set:Set<'t>, subsets: Set<Set<'t>>, least:Set<'t>, greatest:Set<'t>) =
    inherit BooleanAlgebra<Set<'t>>(OrderedSet(subsets, Set.(|<|)), Set.(|+|), Set.(|*|), least, greatest, set.Complement)
    member val Subsets = OrderedSet(subsets, Set.(|<|))       
    interface ITotalOrder<Set<'t>>
    new(set: Set<'t>) = SetAlgebra(set, set.Powerset, Empty, set) 

type SigmaAlgebra<'t when 't: equality>(set: Set<'t>, subsets: Set<Set<'t>>, least:Set<'t>, greatest:Set<'t>) =
    inherit SetAlgebra<'t>(set, subsets, least, greatest)
    new(set: Set<'t>) = SigmaAlgebra(set, set.Powerset, Empty, set)
