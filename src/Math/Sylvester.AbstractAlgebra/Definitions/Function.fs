namespace Sylvester

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

type fn<'d, 'r> = Fn of ('d->'r)
with 
    member x.Apply = let (Fn f) = x in f 
    member x.Item(i:'d) = x.Apply i

[<AutoOpen>]
module Function =
    let fn a = Fn a

    let dom (f:Map<'d, 'r>) = formula<Set<'d>>

    let range (f:Map<'d, 'r>) = formula<Set<'d>>

    let injection = pred<Map<'d, 'r>>

    let surjection = pred<Map<'d, 'r>>

    let bijection = pred<Map<'d, 'r>>

    let increasing = pred<Map<'d, 'r>>

    let increasing' = pred<Map<'d, 'r>>

    let decreasing = pred<Map<'d, 'r>>

    let decreasing' = pred<Map<'d, 'r>>

   