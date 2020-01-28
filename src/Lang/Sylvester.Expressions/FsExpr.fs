namespace Sylvester

open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns
open FSharp.Quotations.ExprShape

[<AutoOpen>]
module FsExpr =
    /// Based on: http://www.fssnip.net/bx/title/Expanding-quotations by Tomas Petricek.
    /// Expand variables and calls to methods and propery getters.
    let expand expr =
        let rec expandv vars expr = 
          let expanded = 
            match expr with
            | WithValue(_, _, e) -> expandv vars e
            | Call(body, MethodWithReflectedDefinition meth, args) ->
                let this = match body with Some b -> Expr.Application(meth, b) | _ -> meth
                let res = Expr.Applications(this, [ for a in args -> [a]])
                expandv vars res
            | PropertyGet(body, PropertyGetterWithReflectedDefinition p, []) -> 
                let this = match body with Some b -> b | None -> p
                expandv vars this
            // If the variable has an assignment, then replace it with the expression
            | ExprShape.ShapeVar v when Map.containsKey v vars -> vars.[v]    
            // Else apply expandv recursively on all sub-expressions
            | ExprShape.ShapeVar v -> Expr.Var v        
            | ExprShape.ShapeLambda(v, expr) -> Expr.Lambda(v, expandv vars expr)
            | ExprShape.ShapeCombination(o, exprs) ->
                ExprShape.RebuildShapeCombination(o, List.map (expandv vars) exprs)

          // After expanding, try reducing the expression - we can replace 'let' expressions and applications where the first argument is lambda.
          match expanded with
          | Application(ExprShape.ShapeLambda(v, body), assign)
          | Let(v, assign, body) ->
                expandv (Map.add v (expandv vars assign) vars) body
          | _ -> expanded

        expandv Map.empty expr


type FsExpr<'t>([<ReflectedDefinition(true)>] expr: Expr<'t>) = 
    member x.Expr = expand expr
  
    

    