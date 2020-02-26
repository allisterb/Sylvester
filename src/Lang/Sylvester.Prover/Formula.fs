namespace Sylph

open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns

open Sylvester

type Formula<'t, 'u>([<ReflectedDefinition(true)>] expr: Expr<'t -> 'u>) =
    let (v, t, e) = expandReflectedDefinitionParam expr
    member val Type = t
    member val Apply = v :?> ('t -> 'u)
    member val Expr = body e
    member val LambdaExpr = e 
    member val Src = decompile e
    member x.Form = (x, x.Apply, x.Expr)
    override x.ToString() = x.Src   
    static member (<=>) (lhs:Formula<'t, bool>, rhs:Formula<'t, bool>) = boolean_axioms(lhs.Expr, rhs.Expr)
    static member (==) (lhs:Formula<'t, 'u>, rhs:Formula<'t, 'u>) = lhs, rhs
    
type F<'t, 'u> = Formula<'t, 'u>

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
