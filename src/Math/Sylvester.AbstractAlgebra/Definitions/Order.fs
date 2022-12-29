namespace Sylvester

/// A set of elements with a partial order relation i.e. an operation that is reflexive, anti-symmetric and transitive.
type IPartialOrder<'t when 't: equality> = 
    inherit ISet<'t>
    inherit IReflexiveRelation<'t>
    inherit IAntiSymmetricRelation<'t>
    inherit ITransitiveRelation<'t>
    
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
type IMaximal<'t when 't : equality> =
    inherit IBoundedAbove<'t>
    abstract Maximal:'t

/// A set that contains at least one of its lower bounds. 
type IMinimal<'t when 't : equality> =
    inherit IBoundedBelow<'t>
    abstract Minimal:'t

/// A set that contains a maximal element greater than or equal to all other maximals.
type IGreatest<'t when 't : equality> =
    inherit IMaximal<'t>
    abstract Greatest:'t

/// A set that contains a minimal element lesser than or equal to all other minimals.
type ILeast<'t when 't : equality> =
    inherit IMinimal<'t>
    abstract Least:'t

/// A totally ordered set where every subset that is bounded below has a least element.
type IWellOrder<'t when 't : equality and 't: comparison> =
    inherit ITotalOrder<'t>
    abstract Least:ISet<'t>->'t

/// A set of elements with a partial order relation.
type Poset<'t when 't: equality>(set:ISet<'t>, order:Order<'t>) = 
    member val Set = set.Set
    member val Order = order
    static member (|?|) (l:'t * 't, r:Poset<'t>) = let op = evaluate r.Order in op (fst l) (snd l)
    interface ISet<'t> with
        member val Set = set.Set
        member x.Equals (y:Set<'t>) = set.Equals y
    interface IPartialOrder<'t> with
        member val Domain = set.Set
        member val CoDomain = set.Set
        member val Op = order
       
/// A set of elements with a total order relation.
type OrderedSet<'t when 't: equality and 't : comparison>(set:ISet<'t>) =
    inherit Poset<'t>(set, <@ (<=) @>)
    interface ITotalOrder<'t>
    