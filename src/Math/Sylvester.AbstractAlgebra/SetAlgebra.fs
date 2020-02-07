namespace Sylvester

type SetAlgebra<'t when 't: equality>(set: Set<'t>, subsets: Set<Set<'t>>) =
    inherit BooleanAlgebra<Set<'t>>(subsets, (|+|), (|*|),  set, Empty, set.Difference)

    new(set: Set<'t>) = SetAlgebra(set, set.Powerset)

