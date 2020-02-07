namespace Sylvester

type BooleanAlgebra<'t when 't: equality>(set: ISet<'t>, join: BinaryOp<'t>, meet: BinaryOp<'t>, greatest:'t, least:'t, complement:UnaryOp<'t>) =
    inherit DistributedComplementedLattice<'t>(set, join, meet, least, greatest, complement)

type SetAlgebra<'t when 't: equality>(set: Set<'t>, subsets: Set<Set<'t>>) =
    inherit BooleanAlgebra<Set<'t>>(subsets, (|+|), (|*|),  set, Empty, set.Difference)