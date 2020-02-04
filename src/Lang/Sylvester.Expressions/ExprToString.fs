namespace Sylvester 

open System
open System.Reflection
open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns

open Swensen.Unquote.Decompilation
// Based on https://github.com/luajalla/snippets/blob/master/TransformExpr.fs by Natallie Baikevich
[<AutoOpen>]
module ExprToString =
    let inline replace name value (map: Map<_, _>) = 
        if (map.ContainsKey name) then 
            Map.remove name map |> Map.add name value
        else
            map |> Map.add name value

    // Custom functions here
    let mduration settlement maturity coupon yld frequency basis = 0M
    let accrint issue first_interest settlement rate par frequency basis = 0M

    /// Get reflected definition for method info 
    let defForMethodInfo() =
        let cache = System.Collections.Generic.Dictionary<_,_>()
        fun mi -> 
            if cache.ContainsKey mi then cache.[mi]
            else
                let res = Expr.TryGetReflectedDefinition mi
                cache.Add (mi, res)
                res

    /// Simplified SpecificCall
    let inline (|Func|_|) expr =
        match expr with
        | Lambdas(_,(Call(_,minfo1,_))) -> function
            | Call(obj, minfo2, args) when minfo1.MetadataToken = minfo2.MetadataToken ->
                Some args
            | _ -> None
        | _ -> failwith "invalid template parameter"

    /// Info for the transformation steps
    type Info = {
        Scope: Map<string, Expr>
        Prior: int
        RightOperand: bool
    }

    let exprToString expr  =  decompile expr


