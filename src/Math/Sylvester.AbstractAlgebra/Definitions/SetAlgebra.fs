namespace Sylvester

type SetAlgebra<'t when 't: equality>(subsets: Set<Set<'t>>, complement: UnaryOp<Set<'t>>) =
    inherit BooleanAlgebra<Set<'t>>(OrderedSet(subsets, Set.(|<|)), Set.(|+|), Set.(|*|), Seq.min subsets, Seq.max subsets, complement)
    member val Subsets = OrderedSet(subsets, Set.(|<|))       
    interface ITotalOrder<Set<'t>>
    new(set: Set<'t>) = SetAlgebra(set.Powerset, set.Complement) 
    new(set: Set<'t>, subsets:Set<Set<'t>>) = SetAlgebra(subsets, set.Complement)

type SigmaAlgebra<'t when 't: equality>(subsets: Set<Set<'t>>, complement: UnaryOp<Set<'t>>) =
    inherit SetAlgebra<'t>(subsets, complement)
    do if not(subsets.HasElement Empty) then failwith "This set of subsets does not contain the empty set as a distinguished element."
    new(set: Set<'t>) = SigmaAlgebra(set.Powerset, set.Complement)
    new(set: Set<'t>, subsets:Set<Set<'t>>) = SigmaAlgebra(subsets, set.Complement)

[<AutoOpen>]
module SetAlgebra' =
    type Set<'t when 't : equality> with
        static member toAlgebra (set:Set<Set<'t>>) = SetAlgebra(set)
        static member toAlgebra(set:Set<'t>, f) = Gen((fun s -> set.Powerset.HasElement s && f s), set.Powerset |> Seq.filter f) |> Set.ofGen |> Set.toAlgebra
        static member toSigmaAlgebra (set:Set<Set<'t>>) = SigmaAlgebra(set)
        static member toSigmaAlgebra (s:Set<'t>) = s.Powerset |> Set.toSigmaAlgebra
        static member toSigmaAlgebra(set:Set<'t>, f) = Gen((fun s -> set.Powerset.HasElement s && f s), set.Powerset |> Seq.filter f) |> Set.ofGen |> Set.toSigmaAlgebra
        static member toLattice set = set :> IComplementedLattice<Set<'t>> 

    let toSetAlgebra f set = Set.toAlgebra(f, set)