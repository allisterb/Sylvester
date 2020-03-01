namespace Sylph

open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns

open Sylvester

type Formula<'t>([<ReflectedDefinition(true)>] expr: Expr<'t>) =
    let (v, t, e) = expandReflectedDefinitionParam expr
    member val Type = t
    member val Apply = v :?> 't
    member val Expr = body e
    member val LambdaExpr = e 
    member val Src = decompile e
    member x.Form = (x, x.Apply, x.Expr)
    override x.ToString() = x.Src   
    static member (==) (lhs:Formula<'t>, rhs:Formula<'t>) = lhs, rhs
    
    static member T = Formula(fun () -> true)
    static member F = Formula(fun () -> false)  
    
    // We want to be able to say formulae with logical form are == True/False also i.e a tautolgy or contradiction so define these overloads too
    static member (==) (lhs:Formula<bool->bool>, rhs:Formula<unit -> bool>) = lhs, rhs
    static member (==) (lhs:Formula<bool->bool->bool>, rhs:Formula<unit -> bool>) = lhs, rhs
    static member (==) (lhs:Formula<bool->bool->bool->bool>, rhs:Formula<unit -> bool>) = lhs, rhs
    static member (==) (lhs:Formula<bool->bool->bool->bool->bool>, rhs:Formula<unit -> bool>) = lhs, rhs

type F<'t> = Formula<'t>
type Prop = Formula<unit->bool>

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
