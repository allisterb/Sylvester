namespace Sylvester

open System.Collections.Generic

open FSharp.Quotations

open MathNet.Symbolics
open MathNetExpr

[<RequireQualifiedAccess>]
module SymbolicOps =    
    let AlgebraicBinaryOps = new Dictionary<string, Dictionary<string, Expr -> Expr -> Expr>>()
    
    let addBinary<'t> name op = AlgebraicBinaryOps.Add(name, op)

    let findAlgebraicBinary<'t> op (l:Expr<'t>) (r:Expr<'t>) = 
        if AlgebraicBinaryOps.ContainsKey typeof<'t>.Name && AlgebraicBinaryOps.[typeof<'t>.Name].ContainsKey op then 
            AlgebraicBinaryOps.[typeof<'t>.Name].[op] l.Raw r.Raw  
        else failwithf "No symbolic %s operation implemented for type %A." op typeof<'t>

    let Add<'t> = findAlgebraicBinary<'t> "Add"
    let Mul<'t> = findAlgebraicBinary<'t> "Mul"
    let Sub<'t> = findAlgebraicBinary<'t> "Sub"
    let Divide<'t> = findAlgebraicBinary<'t> "Divide"
    let Pow<'t> = findAlgebraicBinary<'t> "Pow"

[<AutoOpen>]
module Symbolic =
    let (+.) (l:Expr<'t>) (r:Expr<'t>) = SymbolicOps.Add<'t> l r
    
    let algeb_simplify x = callUnary id x
        
    let algeb_expand x = callUnary Algebraic.expand x
        
    let polyn_coeffs e x = 
        x |> expand 
        |> MathNetExpr.fromQuotation 
        |> Polynomial.coefficients (e |> expand |> fromQuotation) 
        |> Array.map (toQuotation (x |> expand |> get_vars))
        |> Array.map(Option.get)