#load "MathInclude.fsx"

open System 
open System.Linq

open Sylvester

[<RequireQualifiedAccess>] 
[<ReflectedDefinition>]
module op =
    let c = infiniteSeq ((+) 65 >> Char.ConvertFromUtf32)

    let e = (+) 65 >> Char.ConvertFromUtf32

open Microsoft.FSharp.Quotations    
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns

let expand expr =
    let rec expandv vars expr = 
      let expanded = 
        match expr with
        | WithValue(v, t, e) -> expandv vars e
        | Call(body, MethodWithReflectedDefinition meth, args) ->
            let this = match body with Some b -> Expr.Application(meth, b) | _ -> meth
            let res = Expr.Applications(this, [ for a in args -> [a]])
            expandv vars res
        | PropertyGet(body, PropertyGetterWithReflectedDefinition p, []) -> 
            let this = match body with Some b -> b | None -> p
            expandv vars this
        // If the variable has an assignment, then replace it with the expression
        | ExprShape.ShapeVar v when Map.containsKey v vars -> vars.[v]    
        // Apply 'expand' recursively on all sub-expressions
        | ExprShape.ShapeVar v -> Expr.Var v        
        | ExprShape.ShapeLambda(v, expr) -> Expr.Lambda(v, expandv vars expr)
        | ExprShape.ShapeCombination(o, exprs) ->
            ExprShape.RebuildShapeCombination(o, List.map (expandv vars) exprs)

      // After expanding, try reducing the expression - we can replace 'let'
      // expressions and applications where the first argument is lambda
      match expanded with
      | Application(ExprShape.ShapeLambda(v, body), assign)
      | Let(v, assign, body) ->expandv (Map.add v (expandv vars assign) vars) body
      | _ -> expanded

    expandv Map.empty expr

let expand2 expr =
    let rec expandv vars expr = 
      let expanded = 
        match expr with
        | Patterns.WithValue(v, t, e) -> expandv vars e
        | Patterns.Call(body, DerivedPatterns.MethodWithReflectedDefinition meth, args) ->
            let this = match body with Some b -> Expr.Application(meth, b) | _ -> meth
            let res = Expr.Applications(this, [ for a in args -> [a]])
            expandv vars res
        | Patterns.PropertyGet(body, DerivedPatterns.PropertyGetterWithReflectedDefinition p, []) -> 
            let this = match body with Some b -> b | None -> p
            expandv vars this
        | _ -> expr
      // After expanding, try reducing the expression - we can replace 'let'
      // expressions and applications where the first argument is lambda
      match expanded with
      | Patterns.Application(ExprShape.ShapeLambda(v, body), assign)
      | Patterns.Let(v, assign, body) ->expandv (Map.add v (expandv vars assign) vars) body
      | _ -> expanded

    expandv Map.empty expr

//expand (FsExpr(op.e).Expr)

//("D","D") |<| op.c 
 

//c.Take(26) |> Array.ofSeq

let d = Monoid(op.c, (+), "")

d.Take(5) |> Array.ofSeq

(d.["nancy", "drew"]) + "/" 
let h = Groupoids(d, d, (+) "/" )

h.["nancy", "drew"]

//let m = Morph(d,d,id)

//let g = Seq ["A"; "A"; "B"]

//g.Take(30) |> Array.ofSeq

//Integers.Group.Take(10) |> Array.ofSeq