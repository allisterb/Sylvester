namespace Sylvester

// Make Formula an alias for the reflected definition attribute.
type Formula = ReflectedDefinitionAttribute

[<AutoOpen>]
module Formula =    
    (* Logical operators for formulas *)

    [<Unicode("\u2227")>]
    let (|&|) l r = l && r
    [<Unicode("\u2228")>]
    let (|||) l r = l || r
    let (==>) l r = not l || r
    let (<==) l r = r ==> l

    (* Introduce variable names for formulas *)
    
    let var<'t> = Unchecked.defaultof<'t>
    let var2<'t> = Unchecked.defaultof<'t>, Unchecked.defaultof<'t>
    let var3<'t> = Unchecked.defaultof<'t>, Unchecked.defaultof<'t>, Unchecked.defaultof<'t>
    let var4<'t> = Unchecked.defaultof<'t>, Unchecked.defaultof<'t>, Unchecked.defaultof<'t>, Unchecked.defaultof<'t>

    (* Propositions and quantifiers *)

    let prop (text:string) = Unchecked.defaultof<bool>
    [<Unicode("\u2200")>]
    let forall<'u> (bound:'u) (range_body:bool) = Unchecked.defaultof<bool>
    let exists<'u> (bound:'u) (range:bool) (body:bool) = Unchecked.defaultof<bool>
    let sum<'t,'u> (bound:'u) (range:bool) (body:'t) = Unchecked.defaultof<'t>
    let product<'t,'u> (bound:'u) (range:bool) (body:'t) = Unchecked.defaultof<'t>

    let rec FormulaModuleType = getModuleType <@ FormulaModuleType @>