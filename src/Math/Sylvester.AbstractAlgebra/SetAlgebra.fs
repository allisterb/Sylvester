namespace Sylvester

type SetAlgebra<'t when 't: equality>(subsets: Set<Set<'t>>, complement: UnaryOp<Set<'t>>) =
    inherit BooleanAlgebra<Set<'t>>(OrderedSet(subsets), (|+|), (|*|), Seq.min subsets, Seq.max subsets, complement)
    member val Subsets = OrderedSet(subsets)
    member x.AsLattice = x :> IComplementedLattice<Set<'t>>        
    interface ITotalOrder<Set<'t>>
    new(set: Set<'t>) = SetAlgebra(set.Powerset, set.Complement) 
    new(set: Set<'t>, subsets:Set<Set<'t>>) = SetAlgebra(subsets, set.Complement)

type SigmaAlgebra<'t when 't: equality>(subsets: Set<Set<'t>>, complement: UnaryOp<Set<'t>>) =
    inherit SetAlgebra<'t>(subsets, complement)
    do if not(subsets.HasElement Empty) then failwith "This set of subsets does not contain the empty set as a distinguished element."
    new(set: Set<'t>) = SigmaAlgebra(set.Powerset, set.Complement)
    new(set: Set<'t>, subsets:Set<Set<'t>>) = SigmaAlgebra(subsets, set.Complement)

[<AutoOpen>]
module SetAlgebra =
    type Set<'t when 't : equality> with
        member x.AsAlgebra = SetAlgebra(x)
        static member toAlgebra (s:Set<Set<'t>>) = s.AsAlgebra 
        member x.ToAlgebra(f) = Gen((fun s -> x.Powerset.HasElement s && f s), x.Powerset |> Seq.filter f) |> Set.ofGen |> Set.toAlgebra
        member x.AsSigmaAlgebra = SigmaAlgebra(x)
        static member toSigmaAlgebra (s:Set<Set<'t>>) = s.AsSigmaAlgebra
        static member toSigmaAlgebra (s:Set<'t>) = s.Powerset.AsSigmaAlgebra
        member x.ToSigmaAlgebra(f) = Gen((fun s -> x.Powerset.HasElement s && f s), x.Powerset |> Seq.filter f) |> Set.ofGen |> Set.toSigmaAlgebra