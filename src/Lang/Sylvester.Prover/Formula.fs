namespace Sylvester

open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns
open FSharp.Quotations.ExprShape

type Formula<'t>([<ReflectedDefinition>] expr: Expr<'t>) =
    let expandedExpr = expand expr
    member val Expr = expandedExpr
    member val Src = decompile expandedExpr
    override x.ToString() = x.Src
    static member (<=>) (lhs:Formula<'t>, rhs:Formula<'t>) = lhs.Src = rhs.Src

type F<'t> = Formula<'t>

type IdentityGoal<'t>(lhs:Formula<'t>, rhs:Formula<'t>) = 
    let x = typedefof<int>
    member val Lhs = lhs
    member val Rhs = rhs


module ArithmeticPatterns =
    let (|Addition|_|) (expr:Expr) =
        match expr with
        | SpecificCall <@@ (+) @@> (None, _,l::r::[]) -> Some(l, r) 
        | _ -> None

    let (|Subtraction|_|) (expr:Expr) =
        match expr with
        | SpecificCall <@@ (-) @@> (None, _,l::r::[]) -> Some(l, r)
        | _ -> None
    
    let (|Multiplication|_|) (expr:Expr) =
        match expr with
        | SpecificCall <@@ (*) @@> (None, _,l::r::[]) -> Some(l, r)
        | _ -> None


   
[<AutoOpen>]
module ArithmeticRules =
    open ArithmeticPatterns
    let rec reduceConstantIntOperands expr =
        match expr with
        | Addition(Addition(l, Int32 lval), Int32 rval) -> let s = Expr.Value(lval + rval) in <@@ %%l + %%s @@>
        
        | ShapeVar v -> Expr.Var v
        | ShapeLambda (v,e) -> Expr.Lambda (v, reduceConstantIntOperands e)
        | ShapeCombination (o, exprs) -> RebuildShapeCombination (o,List.map reduceConstantIntOperands exprs)



