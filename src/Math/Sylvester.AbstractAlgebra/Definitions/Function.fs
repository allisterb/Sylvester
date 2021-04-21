namespace Sylvester

type IRelation<'t when 't: equality> = 
    inherit ISet<'t>

type IReflexiveRelation<'t when 't : equality> =
    inherit IRelation<'t>

type ISymmetricRelation<'t when 't : equality> =
    inherit IRelation<'t>
    
type IAntiSymmetricRelation<'t when 't : equality> =
    inherit IRelation<'t>

type ITransitiveRelation<'t when 't : equality> =
    inherit IRelation<'t>

type IRelation<'a, 'b when 'a : equality and 'b : equality> = 
    inherit ISet<'a * 'b>

type IReflexiveRelation<'a, 'b when 'a : equality and 'b : equality>  = 
    inherit IRelation<'a, 'b>

type ISymmetricRelation<'a, 'b when 'a : equality and 'b : equality>  = 
    inherit IRelation<'a, 'b>

type IAntiSymmetricRelation<'a, 'b when 'a : equality and 'b : equality>  = 
    inherit IRelation<'a, 'b>

type ITransitiveRelation<'a, 'b when 'a : equality and 'b : equality>  = 
    inherit IRelation<'a, 'b>

type IEquivalenceRelation<'a, 'b when 'a : equality and 'b : equality> =
    inherit IReflexiveRelation<'a, 'b>
    inherit ISymmetricRelation<'a, 'b>
    inherit ITransitiveRelation<'a, 'b>

[<AutoOpen>]
module Function =
    let dom (f:'d->'r) = formula<Set<'d>>

    let range (f:'d->'r) = formula<Set<'r>>

    let relation (f:'d->'r) = formula<IRelation<'d, 'r>>

    let injection = pred<'d->'r>

    let surjection = pred<'d->'r>

    let bijection = pred<'d->'r>

    let increasing = pred<'d->'r>

    let increasing' = pred<'d->'r>

    let decreasing = pred<'d->'r>

    let decreasing' = pred<'d->'r>

    let order = pred<'d->'r>
   