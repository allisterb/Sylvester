namespace Sylvester

type MultiSet<'t when 't: equality> = MultiSet of Set<'t> * seq<int> 

