namespace Sylvester

open FSharp.Quotations

// Make Formula an alias for the reflected definition attribute
type Formula = ReflectedDefinitionAttribute

type Quantifier<'t, 'u> = Quantifier of ('t -> 't -> 't)  * 'u list * bool * 't

[<AutoOpen>]
module Formulas =

    (* Logical operators for formulas *)
 
     [<Unicode("\u2227")>]
     let (|&|) (l:'l) (r:'r) =  
        let lval = 
            match box l with
            | :? bool as b -> b
            | :? Quantifier<bool, 'l> -> false
            | _ -> failwith "The LHS of this expression does not have a truth value."
        let rval = 
             match box r with
             | :? bool as b -> b
             | :? Quantifier<bool, 'r> -> false
             | _ -> failwith "The LHS of this expression does not have a truth value."
        lval && rval

     [<Unicode("\u2228")>]
     let (|||) (l:'l) (r:'r) = 
        let lval = 
            match box l with
            | :? bool as b -> b
            | :? Quantifier<bool, 'l> -> false
            | _ -> failwith "The LHS of this expression does not have a truth value."
        let rval = 
             match box r with
             | :? bool as b -> b
             | :? Quantifier<bool, 'r> -> false
             | _ -> failwith "The RHS of this expression does not have a truth value."
        lval || rval

     let (==>) (l:bool) (r:bool) = (not l) || r
     let (<==) (l:bool) (r:bool) = r ==> l

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
     let (!!) = forall //bound range body = Quantifier((|&|), bound, range, body) 
     [<ReflectedDefinition>]
     let (!?) = exists //bound range body = Quantifier((|||), bound, range, body)
