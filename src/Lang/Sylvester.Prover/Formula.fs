namespace Sylvester

open System

open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns
open FSharp.Quotations.ExprShape

type Formula<'t, 'u>([<ReflectedDefinition(true)>] expr: Expr<'t -> 'u>) =
    let (v, e) = expandReflectedDefinitionParam expr
    member val Obj = v
    member val Apply = v :?> ('t -> 'u)
    member val Expr = e
    member val Src = decompile e
    override x.ToString() = x.Src
    static member (|->) (l: Formula<'t, 'u>, r: 't) = l.Apply r
    static member (<=>) (lhs:Formula<'t, 'u>, rhs:Formula<'t, 'u>) = sequal lhs.Expr rhs.Expr
    static member (<=>) (lhs:Expr, rhs:Formula<'t, 'u>) = sequal lhs rhs.Expr
    static member (<=>) (lhs:Formula<'t, 'u>, rhs:Expr) = sequal lhs.Expr rhs

type F<'t, 'u> = Formula<'t, 'u>

type Value<'t> = Formula<unit, 't>

type Predicate<'t> = Formula<'t, bool>

[<AutoOpen>]
module Formula =
    [<ReflectedDefinition>] 
    let Value (x:'t) = Value(fun () -> x)
    
    let src (f:Formula<'t, 'u>) = decompile f.Expr
    
    let split<'t, 'u> (f:Formula<'t, 'u>) =
        match f.Expr with
        | Lambda(v, Call(None, m, l::r::[])) -> (Expr.Lambda(v, l), Expr.Lambda(v, r))
        | _ -> failwithf "Cannot spilt formula %A." f

module FormulaPatterns =
    let (|BinaryOp|_|) =
        function
        | Call(_, _, l::r::[]) -> Some (l, r)
        | _ -> None

    let (|Add|_|) =
        function
        | SpecificCall <@@ (+) @@> (None,_,l::r::[]) -> Some(l, r) 
        | _ -> None

    let (|Subtract|_|) =
        function
        | SpecificCall <@@ (-) @@> (None,_,l::r::[]) -> Some(l, r)
        | _ -> None
    
    let (|Multiply|_|) (expr:Expr) =
        match expr with
        | SpecificCall <@@ (*) @@> (None,_,l::r::[]) -> Some(l, r)
        | _ -> None
        
    let (|Range|_|) (expr:Expr) =
        match expr with
        | SpecificCall <@@ (..) @@> (None,_,l::r::[]) -> Some(l,r)
        | _ -> None

    let (|Sequence|_|) (expr:Expr) =
        match expr with
        | Call(None, method, Range(l, r)::[]) when method.Name = "CreateSequence" -> Some (l, r)
        | _ -> None

    let (|Sum|_|) (expr:Expr) =
        match expr with
        | Call(None, method, Sequence(l, r)::[]) when method.Name = "Sum" -> Some (l, r)
        | _ -> None

    let (|Assoc|_|) (A, B) =
        match A,B with
        // x + y + z, x + (y + z)
        | Lambda(_,BinaryOp(BinaryOp(a1, a2), a3)), Lambda(_, BinaryOp(b1, BinaryOp(b2, b3))) when (sequal3 a1 a2 a3 b1 b2 b3) -> Some B 
        | Lambda(_, BinaryOp(a1, BinaryOp(a2, a3))), Lambda(_, BinaryOp(BinaryOp(b1, b2), b3)) when (sequal3 a1 a2 a3 b1 b2 b3)-> Some B
        | _ -> None



[<AutoOpen>]
module ArithmeticRules =
    
    open ArithmeticPatterns
    
    let rec reduceConstantIntOperands (expr:Expr) =
        match expr with
        | Add(Add(l, Int32 lval), Int32 rval) -> let s = Expr.Value(lval + rval) in <@@ %%l + %%s @@>
        | _ -> traverse expr reduceConstantIntOperands
    
    
    let rec replaceSum (expr:Expr) =
        match expr with
        | Sum (l, r) -> expr
        | _ -> traverse expr replaceSum

    let g = <@@ replaceSum @@>
    let gg = g.Substitute(fun v -> if v.Name = "x" then Some (Expr.Var(Var.Global("x", v.Type))) else None)