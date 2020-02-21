namespace Sylph

open Microsoft.FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns

open Sylvester

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
    let (|Equal|_|) =
        function
        | (A, B) when sequal A B -> Some true
        | _ -> None

    let (|UnaryOp|_|) =
        function
        | Call(_, _, l::[]) -> Some l
        | _ -> None

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
