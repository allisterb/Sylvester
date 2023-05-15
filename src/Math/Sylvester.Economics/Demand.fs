namespace Sylvester

type DemandFunction(map:MapExpr<real, real>) = 
    inherit RealFunction(map)

[<AutoOpen>]
module Demand =
    let demand_fun map = DemandFunction map
