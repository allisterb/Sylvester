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
   