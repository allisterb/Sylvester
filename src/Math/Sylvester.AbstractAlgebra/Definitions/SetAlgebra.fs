namespace Sylvester

type SetAlgebra<'t when 't: equality>(set:ISet<'t>, subsets: ISet<Set<'t>>, least:Set<'t>, greatest:Set<'t>) =
    inherit BooleanAlgebra<Set<'t>>(OrderedSet(subsets), Set.set_union, Set.set_intersection, least, greatest, set.Set.Complement)
    member val Set = set.Set
    member val Subsets = OrderedSet(subsets)       
    interface ITotalOrder<Set<'t>>
    new(set: ISet<'t>) = SetAlgebra(set.Set, set.Set.Powerset, Empty, set.Set) 

type SigmaAlgebra<'t when 't: equality>(set: ISet<'t>, subsets: ISet<Set<'t>>, least:Set<'t>, greatest:Set<'t>) =
    inherit SetAlgebra<'t>(set, subsets, least, greatest)
    new(set: ISet<'t>) = SigmaAlgebra(set, set.Set.Powerset, Empty, set.Set)

