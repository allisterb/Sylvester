namespace Sylvester

open FSharp.Quotations

// Make Formula an alias for the reflected definition attribute.
type Formula = ReflectedDefinitionAttribute

[<Formula>]
type Quantifier<'t, 'u> = Quantifier of ('t -> 't -> 't)  * 'u * bool * 't with 
    interface IQuantifier
and IQuantifier = interface end

[<AutoOpen>]
module Formulas =    
    let private get_bool_val (l:'l) (r: 'r) =
        let lval = 
            match box l with
            | :? bool as b -> b
            | :? IQuantifier -> false
            | _ -> failwith "The LHS of this expression does not have a truth value."
        let rval = 
             match box r with
             | :? bool as b -> b
             | :? IQuantifier -> false
             | _ -> failwith "The RHS of this expression does not have a truth value."
        lval, rval

    (* Logical operators for formulas *)

    [<Unicode("\u2227")>]
    let (|&|) (l:'l) (r:'r) =
        let lval, rval = get_bool_val l r
        lval && rval

    [<Unicode("\u2228")>]
    let (|||) (l:'l) (r:'r) = 
        let lval, rval = get_bool_val l r      
        lval || rval

    let (==>) (l:bool) (r:bool) = 
        let lval, rval = get_bool_val l r
        (not lval) || rval

    let (<==) (l:bool) (r:bool) = 
        let lval, rval = get_bool_val l r
        rval ==> lval

    (* Introduce variable names for formulas *)
    
    let var<'t> = Unchecked.defaultof<'t>
    let var2<'t> = Unchecked.defaultof<'t>, Unchecked.defaultof<'t>
    let var3<'t> = Unchecked.defaultof<'t>, Unchecked.defaultof<'t>, Unchecked.defaultof<'t>
    let var4<'t> = Unchecked.defaultof<'t>, Unchecked.defaultof<'t>, Unchecked.defaultof<'t>, Unchecked.defaultof<'t>  
 
    (* Quantifiers*)

    [<ReflectedDefinition>]
    let forall bound range body = Quantifier((|&|), bound, range, body)
    [<ReflectedDefinition>]
    let exists bound range body = Quantifier((|||), bound, range, body)

    [<ReflectedDefinition>]
    let (!!) = forall 
    [<ReflectedDefinition>]
    let (!?) = exists
