namespace Sylvester

type BooleanAlgebra<'t when 't: equality and 't: comparison>(set: ISet<'t>, join: BinaryOp<'t>, meet: BinaryOp<'t>, least:'t, greatest:'t, complement:UnaryOp<'t>) =
    inherit DistributedComplementedLattice<'t>(set, join, meet, least, greatest, complement)