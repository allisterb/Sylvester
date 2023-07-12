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
    let (===>) l r = not l || r
    let (<===) l r = r ===> l
 
    /// Represents a symbolic formula.
    let formula<'t> = Unchecked.defaultof<'t>

    /// Result of symbolic truth-functional operation.
    let truth_value = formula<bool>

    /// Represents a predicate
    let pred<'t> = (fun (_:'t) -> truth_value)
    
    /// Create a predicate with a name
    let pred'<'t> n = 
        let var = Expr.Var(Var(n, typeof<'t -> bool>)) in <@ %%var:'t->bool @>

    let predi<'a, 't> (x:'a) = (box x) :? 't

    (* Quantifiers *)

    /// Generic quantifier for binary op that is symmetric, associative and has an identity.
    let quantifier<'t,'u> (op:'t -> 't -> 't) (bound:'u) (range:bool) (body:'t) = formula<'t>

    [<Symbol "\u2200">]
    let forall<'u> (bound:'u) (range:bool) (body:bool) = truth_value
    [<ReflectedDefinition>]
    let forall'<'u> (bound:'u) (body:bool) = forall bound true body
    [<Symbol "\u2203">]
    let exists<'u> (bound:'u) (range:bool) (body:bool) = truth_value
    [<ReflectedDefinition>]
    let exists'<'u> (bound:'u) (body:bool) = exists bound true body

    /// Generic quantifier with sum semantics.
    let sum<'t,'u> (op:'t -> 't -> 't) (symbol: string) (bound:'u) (range:bool) (body:'t) = formula<'t>
    /// Generic quantifier with product semantics.
    let product<'t,'u> (op:'t -> 't -> 't) (symbol: string) (bound:'u) (range:bool) (body:'t) = formula<'t>

    // Functions
    let func<'s, 't> = fun (_:'s) -> Unchecked.defaultof<'t>
    
    let func2<'r, 's, 't> = fun (_:'r) (_:'s) -> Unchecked.defaultof<'t>