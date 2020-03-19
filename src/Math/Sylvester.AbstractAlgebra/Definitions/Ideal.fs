namespace Sylvester

type IIdeal<'t when 't : equality> =
    inherit IGroup<'t>
    abstract Op:BinaryOp<'t>