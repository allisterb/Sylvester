namespace Sylvester.AbstractAlgebra

open System
open System.Collections
open System.Collections.Generic
open Microsoft.FSharp.Collections

type Numeric<'t when 't : struct  and 't: comparison and 't: equality and 't :> IFormattable> = 't

/// Set of numbers belonging to the continuum of real numbers
type R<'t when 't : struct  and 't: comparison and 't: equality and 't :> IFormattable> =
| R of 't
| Seq of seq<'t>
| Set of ('t -> bool)
    
with 
    interface IEnumerable<'t> with
        member x.GetEnumerator () =
            match x with
            |R a -> let s = seq { yield a} in s.GetEnumerator()
            |Seq s -> s.GetEnumerator()
            |Set s -> failwith "Cannot enumerate an arbitrary subset of R. Use a sequence instead."
                
    interface IEnumerable with
        member x.GetEnumerator () = (x :> IEnumerable<'t>).GetEnumerator () :> IEnumerator

    member x.Sub(f: 't -> bool) = 
        match x with
        |R a -> Set(fun x -> x = a)
        |Seq s -> Seq(s |> Seq.filter f)
        |Set s -> Set(fun x -> s(x) && f(x))

    static member inline (+) (l, r) = 
        match (l, r) with
        |(R a, R b) -> R(a + b)
        |(Seq a, Seq b) -> Seq.concat([a; b]) |> Seq
        |(Set a, Set b) -> Set(fun x -> a(x) || b(x))
           
/// Cartesian product of R x R
type R2<'t when 't:> ValueType and 't : struct  and 't: comparison and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable> =
| R2 of 't * 't
| Seq2 of seq<'t * 't>
| Set2 of (('t * 't) -> bool)

/// Cartesian product of R x R x R
type R3<'t when 't:> ValueType and 't : struct  and 't: comparison and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable> =
| R3 of 't * 't * 't
| Seq3 of seq<'t * 't * 't>
| Set3 of (('t * 't * 't) -> bool)

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
module Integers = 
    let Z = Set(fun (_:int) -> true)
    let Zpos = Seq(infseq (fun n -> n))
    let Zneg = Seq(infseq (fun n -> -1 * n)) 
    let Zplus = Zpos
    let Zminus = Zneg
