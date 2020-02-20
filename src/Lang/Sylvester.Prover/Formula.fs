namespace Sylvester

open Microsoft.FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns

type Formula<'t, 'u>([<ReflectedDefinition(true)>] expr: Expr<'t -> 'u>) =
    let (v, t, e) = expandReflectedDefinitionParam expr
    member val Apply = v :?> ('t -> 'u)
    member val Expr = e
    member val Body = e |> body
    member val Src = decompile e
    member x.Form = (x, x.Apply, x.Body)
    override x.ToString() = x.Src
    
    static member T = Prop(fun() -> true)
    static member F = Prop(fun() -> false)
    static member (<=>) (lhs:Formula<'t, 'u>, rhs:Formula<'t, 'u>) = lhs, rhs
    static member (|-) (lhs:Prop, rhs:Prop) = Implies(lhs, rhs), Prop.T

and Value<'t> = Formula<unit, 't>

and Predicate<'t> = Formula<'t, bool>

and Prop = Value<bool>

and Implies<'a, 'b> = Implies of Prop * Prop with
    member x.Left = let (Implies(a, b)) = x in a
    member x.Right = let (Implies(a, b)) = x in b
    //Todo: member x.Expr = Expr

type F<'t, 'u> = Formula<'t, 'u>

[<AutoOpen>]
module Formula =
    [<ReflectedDefinition>] 
    let value (x:'t) = Value(fun () -> x)
    
    let prop (x:bool) = value x

module FormulaPatterns =
    let (|UnaryOp|_|) =
        function
        | Call(_, _, l::[]) -> Some l
        | _ -> None

    let (|BinaryOp|_|) =
        function
        | Call(_, _, l::r::[]) -> Some (l, r)
        | _ -> None

    let (|Equal|_|) (l:Expr, r:Expr) =
        match (l, r) with
        | (A, B) when sequal A B -> Some (A, B)
        | _ -> None

    // x + y, y + x
    let (|Commute|_|) (A, B) =
        match A,B with
        | Lambda(_, BinaryOp(a1, a2)), Lambda(_, BinaryOp(b1, b2)) when sequal2 a1 a2 b2 b1 -> Some (A, B)
        | BinaryOp(a1, a2), BinaryOp(b1, b2) when sequal2 a1 a2 b2 b1 -> Some (A, B)
        | _ -> None

    // x + y + z, x + (y + z)
    let (|Assoc|_|) (A, B) =
        match A, B with
        | Lambda(_,BinaryOp(BinaryOp(a1, a2), a3)), Lambda(_, BinaryOp(b1, BinaryOp(b2, b3))) when (sequal3 a1 a2 a3 b1 b2 b3) -> Some (A, B)
        | Lambda(_, BinaryOp(a1, BinaryOp(a2, a3))), Lambda(_, BinaryOp(BinaryOp(b1, b2), b3)) when (sequal3 a1 a2 a3 b1 b2 b3)-> Some (A, B)
        
        | BinaryOp(BinaryOp(a1, a2), a3), BinaryOp(b1, BinaryOp(b2, b3)) when (sequal3 a1 a2 a3 b1 b2 b3) -> Some (A, B)
        | BinaryOp(a1, BinaryOp(a2, a3)), BinaryOp(BinaryOp(b1, b2), b3) when (sequal3 a1 a2 a3 b1 b2 b3)-> Some (A, B)
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
