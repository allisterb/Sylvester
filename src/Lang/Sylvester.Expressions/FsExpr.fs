namespace Sylvester

open System
open System.Reflection
open System.Text.RegularExpressions
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations

open Swensen.Unquote
open Swensen.Unquote.Decompilation
open Swensen.Utils

module P = Microsoft.FSharp.Quotations.Patterns
module DP = Microsoft.FSharp.Quotations.DerivedPatterns

module EP = Swensen.Unquote.ExtraPatterns
module ER = Swensen.Unquote.ExtraReflection
module OP = Swensen.Unquote.OperatorPrecedence
module CC = Swensen.Unquote.Decompilation.CustomContext

open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns
open FSharp.Quotations.ExprShape
    
type FsExpr<'t>([<ReflectedDefinition(true)>] expr: Expr<'t>) = 
    member x.Expr = expr
    member x.Decompile((processor:FsStmt -> unit)) =
        let stmtProcessor (str: string, parameters: obj list) =
            let stripFormatting s =
                let i = ref -1
                let eval (rxMatch: Match) =
                    incr i
                    sprintf "@p%d" !i
                Regex.Replace(s, "%.", eval)
            let stmt = stripFormatting str
            FsStmt(stmt, parameters)
        ()
            

  
[<AutoOpen>]
module FsExpr =
    /// Based on: http://www.fssnip.net/bx/title/Expanding-quotations by Tomas Petricek.
    /// Expand variables and calls to methods and propery getters.
    let expand expr =
        let rec rexpand vars expr = 
          let expanded = 
            match expr with
            | WithValue(_, _, e) -> rexpand vars e
            | Call(body, MethodWithReflectedDefinition meth, args) ->
                let this = match body with Some b -> Expr.Application(meth, b) | _ -> meth
                let res = Expr.Applications(this, [ for a in args -> [a]])
                rexpand vars res
            | PropertyGet(body, PropertyGetterWithReflectedDefinition p, []) -> 
                let this = match body with Some b -> b | None -> p
                rexpand vars this
            // If the variable has an assignment, then replace it with the expression
            | ExprShape.ShapeVar v when Map.containsKey v vars -> vars.[v]    
            // Else apply rexpand recursively on all sub-expressions
            | ExprShape.ShapeVar v -> Expr.Var v        
            | ExprShape.ShapeLambda(v, expr) -> Expr.Lambda(v, rexpand vars expr)
            | ExprShape.ShapeCombination(o, exprs) ->
                ExprShape.RebuildShapeCombination(o, List.map (rexpand vars) exprs)

          // After expanding, try reducing the expression - we can replace 'let' expressions and applications where the first argument is lambda.
          match expanded with
          | Application(ExprShape.ShapeLambda(v, body), assign)
          | Let(v, assign, body) ->
                rexpand (Map.add v (rexpand vars assign) vars) body
          | _ -> expanded

        rexpand Map.empty expr
    

    