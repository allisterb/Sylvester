namespace Sylvester

type SetAlgebra<'t when 't: equality>(set:Set<'t>, subsets: Set<Set<'t>>) =
    inherit BooleanAlgebra<Set<'t>>(OrderedSet(subsets, Set.(|<|)), Set.(|+|), Set.(|*|), Seq.min subsets, Seq.max subsets, set.Complement)
    member val Subsets = OrderedSet(subsets, Set.(|<|))       
    interface ITotalOrder<Set<'t>>
    new(set: Set<'t>) = SetAlgebra(set, set.Powerset) 
  
type SigmaAlgebra<'t when 't: equality>(set: Set<'t>, subsets: Set<Set<'t>>) =
    inherit SetAlgebra<'t>(set, subsets)
    do if not(subsets.HasElement Empty) then failwith "This set of subsets does not contain the empty set as a distinguished element."
    new(set: Set<'t>) = SigmaAlgebra(set, set.Powerset)
    

[<AutoOpen>]
module SetAlgebra' =
    type Set<'t when 't : equality> with
        static member toAlgebra(f, set:Set<'t>) = 
            let subsets = Gen((fun s -> set.Powerset.HasElement s && f s), set.Powerset |> Seq.filter f) |> Set.ofGen in
            SetAlgebra(set, subsets)
        static member toSigmaAlgebra (set:Set<'t>) = SigmaAlgebra(set)
        static member toSigmaAlgebra(set:Set<'t>, f) = 
            let subsets = Gen((fun s -> set.Powerset.HasElement s && f s), set.Powerset |> Seq.filter f) |> Set.ofGen in
            SigmaAlgebra(set, subsets)