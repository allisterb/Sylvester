namespace Sylvester

type BooleanAlgebra<'t when 't: equality>(set: ISet<'t>, join: BinaryOp<'t>, meet: BinaryOp<'t>, greatest:'t, least:'t, complement:UnaryOp<'t>) =
    inherit DistributedComplementedLattice<'t>(set, join, meet, least, greatest, complement)