namespace Sylvester

open System.Collections

/// A set of elements with a partial order relation i.e. an operation that is reflexive, anti-symmetric and transitive.
type IPartialOrder<'t when 't: equality> = 
    inherit ISet<'t>
    abstract Order: Order<'t>
    
/// A set of elements with a total order.
type ITotalOrder<'t when 't: equality and 't : comparison> = inherit IPartialOrder<'t>

/// A partially ordered set of elements bounded above.
type IBoundedAbove<'t when 't : equality> =
    inherit IPartialOrder<'t>
    abstract UpperBound: 't

/// A partially ordered set of elements bounded below.
type IBoundedBelow<'t when 't : equality> =
    inherit IPartialOrder<'t>
    abstract LowerBound: 't

/// A bounded partially ordered set.
type IBounded<'t when 't : equality> =
    inherit IBoundedAbove<'t>
    inherit IBoundedBelow<'t>

/// A partially ordered set of elements that has a least upper bound.
type ILeastUpperBound<'t when 't : equality> =
    inherit IBoundedAbove<'t>
    abstract Supremum: 't

/// A partially ordered set of elements that has a greatest lower bound.
type IGreatestLowerBound<'t when 't : equality> =
    inherit IBoundedBelow<'t>
    abstract Infimum: 't

/// A set that contains at least one of its upper bounds. 
type IHasMaximal<'t when 't : equality> =
    inherit IBoundedAbove<'t>
    abstract Maximal:'t

/// A set that contains at least one of its lower bounds. 
type IHasMinimal<'t when 't : equality> =
    inherit IBoundedBelow<'t>
    abstract Minimal:'t

/// A set that contains a maximal element greater than or equal to all other maximals.
type IHasGreatest<'t when 't : equality> =
    inherit IHasMaximal<'t>
    abstract Greatest:'t

/// A set that contains a minimal element lesser than or equal to all other minimals.
type IHasLeast<'t when 't : equality> =
    inherit IHasMinimal<'t>
    abstract Least:'t

/// A totally ordered set where every subset that is bounded below has a least element.
type IWellOrder<'t when 't : equality and 't: comparison> =
    inherit ITotalOrder<'t>
    abstract Least:Set<'t>->'t

/// A set of elements with a partial order relation.
type Poset<'t when 't: equality>(set:ISet<'t>, order:Order<'t>) = 
    member val Set = set.Set
    member val Order = order
    member x.Item(l:'t, r:'t) = x.Order l r
    interface IPartialOrder<'t> with
        member val Set = set.Set
        member val Order = order
        member x.Equals y = x.Set.Equals y

/// A set of elements with a total order relation.
type OrderedSet<'t when 't: equality and 't : comparison>(set:ISet<'t>, order: Order<'t>) =
    inherit Poset<'t>(set, order)
    interface ITotalOrder<'t>