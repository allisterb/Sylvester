namespace Sylvester

type SetAlgebra<'t when 't: equality>(set:Set<'t>, subsets: Set<Set<'t>>) =
    inherit BooleanAlgebra<Set<'t>>(OrderedSet(subsets, Set.(|<|)), Set.(|+|), Set.(|*|), Seq.min subsets, Seq.max subsets, set.Complement)
    member val Subsets = OrderedSet(subsets, Set.(|<|))       
    interface ITotalOrder<Set<'t>>
    new(set: Set<'t>) = SetAlgebra(set, set.Powerset) 

type SigmaAlgebra<'t when 't: equality>(set: Set<'t>, subsets: Set<Set<'t>>) =
    inherit SetAlgebra<'t>(set, subsets)
    new(set: Set<'t>) = SigmaAlgebra(set, set.Powerset)
