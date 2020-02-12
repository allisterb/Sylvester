namespace Sylvester

type SetAlgebra<'t when 't: equality>(set:Set<'t>, subsets: Set<Set<'t>>) =
    inherit BooleanAlgebra<Set<'t>>(OrderedSet(subsets), (|+|), (|*|), Seq.min subsets, Seq.max subsets, set.Complement)
    member val Subsets = OrderedSet(subsets)
    member x.AsLattice = x :> IComplementedLattice<Set<'t>>        
    interface ITotalOrder<Set<'t>>
    new(set: Set<'t>) = SetAlgebra(set, set.Powerset) 

type SigmaAlgebra<'t when 't: equality>(set:Set<'t>, subsets: Set<Set<'t>>) =
    inherit SetAlgebra<'t>(set, subsets)
    do if not(subsets.HasElement set) then failwith "This set of subsets does not contain the parent set as a distinguished element."
    do if not(subsets.HasElement Empty) then failwith "This set of subsets does not contain the empty set as a distinguished element."
    new(set: Set<'t>) = SigmaAlgebra(set, set.Powerset)

[<AutoOpen>]
module SetAlgebra =
    type Set<'t when 't : equality> with
        member x.AsAlgebra = SetAlgebra(x)
        static member ToAlgebra (s:Set<Set<'t>>) = s.AsAlgebra 
        member x.ToAlgebra(f) = Gen((fun s -> x.Powerset.HasElement s && f s), x.Powerset |> Seq.filter f) |> Set.ofGen |> Set.ToAlgebra
        member x.AsSigmaAlgebra = SigmaAlgebra(x)
        static member ToSigmaAlgebra (s:Set<Set<'t>>) = s.AsSigmaAlgebra
        static member ToSigmaAlgebra (s:Set<'t>) = s.Powerset.AsSigmaAlgebra
        member x.ToSigmaAlgebra(f) = Gen((fun s -> x.Powerset.HasElement s && f s), x.Powerset |> Seq.filter f) |> Set.ofGen |> Set.ToSigmaAlgebra