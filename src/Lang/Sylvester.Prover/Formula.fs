namespace Sylph

open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns

open Sylvester

type Formula<'u, 'v>(e: Expr<'u -> 'v>) =
    member val Type = e.Type
    member val Expr = body e
    member val LambdaExpr = e 
    member val Src = decompile e |> replaceCommonLogicalSymbols
    member val Text = e |> body |> src |> replaceCommonLogicalSymbols
    member x.Form = (x, x.Expr)
    override x.ToString() = x.Text  
    static member (==) (lhs:Formula<_,_>, rhs:Formula<_,_>) = lhs, rhs  
    static member T = Formula <@ fun () -> true @>
    static member F = Formula <@ fun () -> false @>
type F<'u, 'v> = Formula<'u, 'v>

module FormulaPatterns =
    let (|UnaryOp|_|) =
        function
        | Call(None, _, l::[]) -> Some l
        | _ -> None

    let (|BinaryOp|_|) =
        function
        | Call(None, _, l::r::[]) -> Some (l, r)
        | _ -> None

    let (|Add|_|) =
        function
        | SpecificCall <@@ (+) @@> (None,_,l::r::[]) -> Some(l, r)
        | _ -> None

    let (|Subtract|_|) =
        function
        | SpecificCall <@@ (-) @@> (None,_,l::r::[]) -> Some(l, r)
        | _ -> None
    
    let (|Multiply|_|) =
        function
        | SpecificCall <@@ (*) @@> (None,_,l::r::[]) -> Some(l, r)
        | _ -> None
        
    let (|Range|_|) =
        function
        | SpecificCall <@@ (..) @@> (None,_,l::r::[]) -> Some(l,r)
        | _ -> None

    let (|Sequence|_|) =
        function
        | Call(None, method, Range(l, r)::[]) when method.Name = "CreateSequence" -> Some (l, r)
        | _ -> None

    let (|Sum|_|)  =
        function
        | Call(None, method, Sequence(l, r)::[]) when method.Name = "Sum" -> Some (l, r)
        | _ -> None

 module Formula =     
     let src expr = decompile expr
