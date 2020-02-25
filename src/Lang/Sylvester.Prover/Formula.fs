namespace Sylph

open FSharp.Quotations
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
    static member (==) (lhs:Formula<'t, 'u>, rhs:Formula<'t, 'u>) = lhs, rhs

type F<'t, 'u> = Formula<'t, 'u>

module FormulaPatterns =
    let (|Equal|_|) =
        function
        | (A, B) when sequal A B -> Some true
        | _ -> None

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
     open FormulaPatterns

     let left (s:Expr -> Expr) = 
        function
        | BinaryOp(l, _) -> l |> s
        | _ -> failwith "Not binary."
     
     let right (s:Expr -> Expr) = 
        function
        | BinaryOp(_, r) -> r |> s
        | _ -> failwith "Not binary."

     let src expr = decompile expr

        

 