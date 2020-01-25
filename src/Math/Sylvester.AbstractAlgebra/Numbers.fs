namespace Sylvester.AbstractAlgebra

open System
open System.Collections
open System.Collections.Generic
open System.Numerics

open Microsoft.FSharp.Collections

/// A set of numbers defined over the elements of some field like R or C.
type Set<'t when 't : struct  and 't: equality and 't :> IFormattable> =

/// A single element of a set of numbers.
| Elem of 't

/// A sequence of numbers i.e. a function from N -> t .
| Seq of seq<'t>

/// A set of numbers defined by a predicate.
| Set of ('t -> bool)
    
with 
    interface IEnumerable<'t> with
        member x.GetEnumerator () =
            match x with
            |Elem a -> let s = seq { yield a} in s.GetEnumerator()
            |Seq s -> s.GetEnumerator()
            |Set s -> failwith "Cannot enumerate an arbitrary subset of numbers. Use a sequence instead."
                
    interface IEnumerable with
        member x.GetEnumerator () = (x :> IEnumerable<'t>).GetEnumerator () :> IEnumerator

    /// A subset of the set.
    member x.Sub(f: 't -> bool) = 
        match x with
        |Elem a -> failwith "Cannot take a subset of a set element."
        |Seq s -> Seq(s |> Seq.filter f)
        |Set s -> Set(fun x -> s(x) && f(x))

    static member inline Empty = Set(fun (_:'t) -> false)

    static member inline (+) (l, r) = 
        match (l, r) with
        |(Elem a, Elem b) -> Elem(a + b)
        |(Elem a, Seq b) -> Seq.append ([a]) (b) |> Seq
        |(Elem a, Set b) -> Set(fun x -> a = x || b(x))

        |(Seq a, Seq b) -> Seq.concat([a; b]) |> Seq
        |_ -> failwith "Not implemented"

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