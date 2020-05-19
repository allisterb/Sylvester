namespace Sylvester

open FSharp.Quotations

// Make Formula an alias for the reflected definition attribute
type Formula = ReflectedDefinitionAttribute

type Quantifier<'t, 'u>(op: Expr<'t -> 't -> 't>, bound: Expr<'u> list, range: Expr<bool>, body:Expr<'t>) =
    let _op, _bound, _range, _body = expand op, bound |> List.map expand, expand range, expand body
    member val Op = _op
    member val Bound =  _bound
    member val Range = _range
    member val Body = _body

[<AutoOpen>]
module Formulas =
    (* Logical operators for formulas *)
 
     [<Unicode("\u2227")>]
     let (|&|) (l:bool) (r:bool) = l && r
     [<Unicode("\u2228")>]
     let (|||) (l:bool) (r:bool) = l || r
     let (==>) (l:bool) (r:bool) = (not l) || r
     let (<==) (l:bool) (r:bool) = r ==> l

    (* Introduce variable names for formulas *)
    
     let var<'t> = Unchecked.defaultof<'t>
     let var2<'t> = Unchecked.defaultof<'t>, Unchecked.defaultof<'t>
     let var3<'t> = Unchecked.defaultof<'t>, Unchecked.defaultof<'t>, Unchecked.defaultof<'t>
     let var4<'t> = Unchecked.defaultof<'t>, Unchecked.defaultof<'t>, Unchecked.defaultof<'t>, Unchecked.defaultof<'t>  
 
    (* Quantifiers*)

     let for_all bound range body = Quantifier(<@ (|&|) @>, bound, range, body)
     let exists bound range body = Quantifier(<@ (|||) @>, bound, range, body)

     let (!!) (bound:Expr<'t> list) (body:Expr<bool>) = for_all bound <@ true @> body
     let (!?) (bound:Expr<'t> list) (body:Expr<bool>) = exists bound <@ true @> body
