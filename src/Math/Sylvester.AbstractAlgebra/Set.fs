namespace Sylvester

open System
open System.Collections
open System.Collections.Generic
open System.Numerics

/// A set of elements belonging to a universe denoted by U.
type Set<'U when 'U: equality> =
/// The empty set
| Empty

/// A single element of U.
| Elem of 'U

/// A sequence of elements i.e. a function from N -> U.
| Seq of seq<'U>

/// A set of elements of U defined by a predicate.
| Set of ('U -> bool)

/// The Cartesian product of 2 sets.
| Prod of Set<'U> * Set<'U>
    
with 
    /// Cartesian product operator.
    static member inline (*) (l, r) = Prod(l, r)
    
    /// Union operator.
    static member inline (|+|) (l, r) = 
        match (l, r) with
        |(Empty, x) -> x
        |(x, Empty) -> x
        
        |(Elem a, Elem b) -> Elem(a + b)
        |(Elem a, Seq b) -> Seq.append ([a]) (b) |> Seq
        |(Elem a, Set b) -> Set(fun x -> a = x || b(x))
        
        |(Seq a, Seq b) -> Seq.concat([a; b]) |> Seq
        |_ -> failwith "Not implemented"

    interface IEnumerable<'U> with
        member x.GetEnumerator () = 
            match x with
            |Empty -> Seq.empty.GetEnumerator()
            |Elem a -> Seq.singleton(a).GetEnumerator()
            |Seq s -> s.GetEnumerator()
            |Set s -> failwith "Cannot enumerate an arbitrary set. Use a sequence instead."

            |Prod(Empty, Empty) -> (Empty :> IEnumerable<'U>).GetEnumerator()
            |Prod(Empty, a) -> (a :> IEnumerable<'U>).GetEnumerator()
            |Prod(a, Empty) -> (a :> IEnumerable<'U>).GetEnumerator()
            

            | _ -> failwith "Not implemented."
                
    interface IEnumerable with
        member x.GetEnumerator () = (x :> IEnumerable<'U>).GetEnumerator () :> IEnumerator

    /// A subset of the set.
    member x.Sub(f: 'U -> bool) = 
        match x with
        |Empty -> failwith "The empty set has no subsets."
        |Elem a -> failwith "A single element has no subsets."
        |Seq s -> Seq(s |> Seq.filter f)
        |Set s -> Set(fun x -> s(x) && f(x))
        | _ -> failwith "Not implemented"
