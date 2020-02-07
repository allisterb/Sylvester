namespace Sylvester

type Measure<'t when 't : equality> = SigmaAlgebra<'t> -> float
