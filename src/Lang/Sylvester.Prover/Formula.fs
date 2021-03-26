namespace Sylvester

open FSharp.Quotations

// Make Formula an alias for the reflected definition attribute.
type Formula = ReflectedDefinitionAttribute

[<AutoOpen>]
module Formula =    
    (* Logical operators for formulas *)

    [<Symbol"\u2227">]
    let (|&|) l r = l && r
    [<Symbol"\u2228">]
    let (|||) l r = l || r
    [<Symbol"\u21D2">]
    let (==>) l r = not l || r
    let (<==) l r = r ==> l

    (* Introduce variable names for formulas *)
    let formula<'t> = Unchecked.defaultof<'t>
    
    let var<'t> = formula<'t>
    let var2<'t> = Unchecked.defaultof<'t>, Unchecked.defaultof<'t>
    let var3<'t> = Unchecked.defaultof<'t>, Unchecked.defaultof<'t>, Unchecked.defaultof<'t>
    let var4<'t> = Unchecked.defaultof<'t>, Unchecked.defaultof<'t>, Unchecked.defaultof<'t>, Unchecked.defaultof<'t>

    let var'<'t> v = Unchecked.defaultof<'t>, Expr.Var(Var(v, typeof<'t>))
    let var2'<'t> v1 v2 = Unchecked.defaultof<'t>, Unchecked.defaultof<'t>, <@ %%(Expr.Var(Var(v1, typeof<'t>))):'t @> , <@ %%(Expr.Var(Var(v2, typeof<'t>))):'t @>
    let var3'<'t> v1 v2 v3 = Unchecked.defaultof<'t>, Unchecked.defaultof<'t>, Unchecked.defaultof<'t>, <@ %%(Expr.Var(Var(v1, typeof<'t>))):'t @>, <@ %%(Expr.Var(Var(v2, typeof<'t>))):'t @>, <@ %%(Expr.Var(Var(v3, typeof<'t>))):'t @>
    let var4'<'t> v1 v2 v3 v4 = Unchecked.defaultof<'t>, Unchecked.defaultof<'t>, Unchecked.defaultof<'t>, Unchecked.defaultof<'t>, <@ %%(Expr.Var(Var(v1, typeof<'t>))):'t @>, <@ %%(Expr.Var(Var(v2, typeof<'t>))):'t @>, <@ %%(Expr.Var(Var(v3, typeof<'t>))):'t @>, <@ %%(Expr.Var(Var(v4, typeof<'t>))):'t @>
    
    (* Propositions and predicates *)

    let prop p1 = var'<bool> p1
    let prop2 p1 p2 = var2'<bool> p1 p2
    let pred<'t> p = (fun (_:'t) -> Unchecked.defaultof<bool>), <@ %%(Expr.Var(Var(p, typeof<'t>))):'t @>
    let pred2<'t> p1 p2 = (fun (_:'t) -> Unchecked.defaultof<bool>), (fun (_:'t) -> Unchecked.defaultof<bool>), <@ %%(Expr.Var(Var(p1, typeof<'t>))):'t @>, <@ %%(Expr.Var(Var(p2, typeof<'t>))):'t @>
    
    (* Quantifiers *)

    /// Generic quantifier for binary op that is symmetric, associative and has an identity.
    let quantifier<'t,'u> (op:'t -> 't -> 't) (bound:'u) (range:bool) (body:'t) = Unchecked.defaultof<'t>

    [<Symbol "\u2200">]
    let forall<'u> (bound:'u) (range:bool) (body:bool) = Unchecked.defaultof<bool>
    [<ReflectedDefinition>]
    let forall'<'u> (bound:'u) (body:bool) = forall bound true body
    [<Symbol "\u2203">]
    let exists<'u> (bound:'u) (range:bool) (body:bool) = Unchecked.defaultof<bool>
    [<ReflectedDefinition>]
    let exists'<'u> (bound:'u) (body:bool) = exists bound true body

    /// Generic quantifier with sum semantics.
    let sum<'t,'u> (op:'t -> 't -> 't) (symbol: string) (bound:'u) (range:bool) (body:'t) = formula<'t>
    /// Generic quantifier with product semantics.
    let product<'t,'u> (op:'t -> 't -> 't) (symbol: string) (bound:'u) (range:bool) (body:'t) = formula<'t>