namespace Sylvester

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
    
    let var<'t> = Unchecked.defaultof<'t>
    let var2<'t> = Unchecked.defaultof<'t>, Unchecked.defaultof<'t>
    let var3<'t> = Unchecked.defaultof<'t>, Unchecked.defaultof<'t>, Unchecked.defaultof<'t>
    let var4<'t> = Unchecked.defaultof<'t>, Unchecked.defaultof<'t>, Unchecked.defaultof<'t>, Unchecked.defaultof<'t>

    (* Propositions and predicates *)

    let prop (sentence:string) = Unchecked.defaultof<bool>
    let predicate<'t> = fun (x:'t) -> Unchecked.defaultof<bool>

    (* Quantifiers *)

    [<Symbol"\u2200">]
    let forall<'u> (bound:'u) (range:bool) (body:bool) = Unchecked.defaultof<bool>
    [<ReflectedDefinition>]
    let forall'<'u> (bound:'u) (body:bool) = forall bound true body
    [<Symbol"\u2203">]
    let exists<'u> (bound:'u) (range:bool) (body:bool) = Unchecked.defaultof<bool>
    [<ReflectedDefinition>]
    let exists'<'u> (bound:'u) (body:bool) = exists bound true body
    
    [<Symbol"\u2211">]
    let sum<'t,'u> (bound:'u) (range:bool) (body:'t) = Unchecked.defaultof<'t>
    let product<'t,'u> (bound:'u) (range:bool) (body:'t) = Unchecked.defaultof<'t>

    