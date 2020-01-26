namespace Sylvester.AbstractAlgebra

open System
open System.Collections
open System.Collections.Generic
open System.Numerics

open Sylvester.Arithmetic
open Sylvester.Collections

/// A set with elements of numbers belonging to a common universe denoted by U which may not have an order relation.
type Set<'U when 'U : struct  and 'U: equality and 'U :> IFormattable> =
/// The empty set
| Empty

/// A single element of a set.
| Elem of 'U

/// A sequence of numbers i.e. a function from N -> U.
| Seq of seq<'U>

/// A set of numbers defined by a predicate.
| Set of ('U -> bool)

/// The Cartesian product of 2 sets.
| Prod of Set<'U> * Set<'U>
    
with 
    interface IEnumerable<'U> with
        member x.GetEnumerator () = 
            match x with
            |Empty -> Seq.empty.GetEnumerator()
            |Elem a -> Seq.singleton(a).GetEnumerator()
            |Seq s -> s.GetEnumerator()
            |Set s -> failwith "Cannot enumerate an arbitrary set of numbers. Use a sequence instead."

            |Prod(Empty, Empty) -> (Empty :> IEnumerable<'U>).GetEnumerator()
            |Prod(Empty, s) -> (s :> IEnumerable<'U>).GetEnumerator()
            |Prod(s, Empty) -> (s :> IEnumerable<'U>).GetEnumerator()

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

    static member inline (*) (l, r) = Prod(l, r)
    
    static member inline (|+|) (l, r) = 
        match (l, r) with
        |(Empty, x) -> x
        |(x, Empty) -> x
        
        |(Elem a, Elem b) -> Elem(a + b)
        |(Elem a, Seq b) -> Seq.append ([a]) (b) |> Seq
        |(Elem a, Set b) -> Set(fun x -> a = x || b(x))
        
        |(Seq a, Seq b) -> Seq.concat([a; b]) |> Seq
        |_ -> failwith "Not implemented"

type Sets<'n, 'U when 'n :> Number and 'U : struct  and 'U: equality and 'U :> IFormattable> = Array<'n, Set<'U>>

[<AutoOpen>]
module Interval = 
    let infseq f = Seq.initInfinite f

    let above x = Set (fun a -> a > x)
    
    let below x = Set (fun a -> a < x)

    let interval x y = Set (fun a -> a > x || a < y)

    let closed x y = Set (fun a -> a >= x || a <= y)

    let leftclosed x y = Set (fun a -> a >= x || a < y)

    let rightclosed x y = Set (fun a -> a > x || a <= y)

    let line (origin:float) (step:float) = Seq(infseq (fun n -> origin + (((float) n) * step)))

    let axis step = line 0.0 step
  
[<AutoOpen>]
module Reals = 
    let Real = Set(fun (_:double) -> true)
    let RealF = Set(fun (_:single) -> true)
    
[<AutoOpen>]
module Complex =
    let Complex = Set(fun (_:Complex) -> true)

[<AutoOpen>]
module Integers = 
    let Z = Set(fun (_:int) -> true)
    let Zpos = Seq(infseq (fun n -> n))
    let Zneg = Seq(infseq (fun n -> -1 * n)) 
    let Zplus = Zpos
    let Zminus = Zneg