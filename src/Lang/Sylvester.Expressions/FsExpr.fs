#nowarn "40"
namespace Sylvester

open System
open System.Collections.Generic
open System.Linq
open System.Reflection
open FSharp.Reflection
open FSharp.Quotations

open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns
open FSharp.Quotations.ExprShape
    
type Delayed<'t> = Lazy<'t>
type DelayedEval<'t> = Lazy<'t>

[<AutoOpen>] 
module FsExpr =
    let src expr = Swensen.Unquote.Operators.decompile expr
    
    let rec dom_count a = if FSharpType.IsFunction a then 1 + dom_count(FSharpType.GetFunctionElements(a) |> snd) else 0
    
    let rec range_type a = if FSharpType.IsFunction a then range_type(FSharpType.GetFunctionElements(a) |> snd) else a

    let is_prop p = range_type p = typeof<bool>
   
    let getFieldInfo = function
    | FieldGet (_, fieldInfo) -> fieldInfo
    | _ -> failwith "Expression is not a field."

    let getPropertyInfo = function
    | PropertyGet (_, info, _) -> info
    | _ -> failwith "Expression is not a property."

    let rec getFuncInfo = function
    | Call(_, methodInfo, _) -> methodInfo
    | Lambda(_, expr) -> getFuncInfo expr
    | Application(Application(Lambda(_, expr), _), _) -> getFuncInfo expr
    | expr -> failwithf "Expression is not a function: %A." expr

    let getModuleType = function
    | PropertyGet (_, info, _) -> info.DeclaringType
    | FieldGet (_, info) -> info.DeclaringType
    | Call (_, info, _) -> info.DeclaringType
    | _ -> failwith "Expression does not have information to retrieve module type."

    let (|Op|_|) (n:string) :MethodInfo->unit option =
        function
        | mi when mi.Name = n -> Some()
        | _ -> None

    let (|UnaryOp|_|) (n:string) =
        let s = "op_" + n in
        function
        | Call (None, Op s, l::[]) -> Some l
        | _ -> None

    let (|BinaryOp|_|) (n:string) =
        let s = "op_" + n in
        function
        | Call (None, Op s, [l;r]) -> Some(l, r)
        | _ -> None

    let (|Prop|_|) (n:string) :PropertyInfo->unit option =
        function
        | mi when mi.Name = n -> Some()
        | _ -> None

    let (|Number|_|) expr =
           match expr with
           | Value(_, _)  -> Some expr
           | _ -> None
    
    let (|Constant|_|) expr =
        match expr with 
        | Value(_, _) 
        | ValueWithName(_,_,_) -> Some expr
        | _ -> None
    
    let (|Variable|_|) expr =
        match expr with 
        | Var _ -> Some expr
        | _ -> None

    let (|VariableWithName|_|) name expr =
        match expr with 
        | Var v when v.Name  = name-> Some expr
        | _ -> None

    let (|VariableWithOneOfNames|_|) (names:string seq) expr =
        match expr with 
        | Var v when Seq.contains v.Name names -> Some expr
        | _ -> None

    let (|Atom|_|) expr =
        match expr with
        | Variable _ 
        | Constant _ 
        | Number _ -> Some expr
        | _ -> None

    let (|Negation|_|) =
        function
        | UnaryOp "UnaryNegation" l-> Some l 
        | _ -> None

    let (|Addition|_|) =
        function
        | BinaryOp "Addition" (l,r)-> Some(l, r) 
        | _ -> None
    
    let (|Subtraction|_|) =
        function
        | BinaryOp "Subtraction" (l,r)-> Some(l, r)  
        | _ -> None

    let (|Multiplication|_|) =
        function
        | BinaryOp "Multiply" (l,r)-> Some(l, r)  
        | _ -> None

    let (|Division|_|) =
        function
        | BinaryOp "Divide" (l,r)-> Some(l, r)  
        | _ -> None

    let (|Exp|_|) =
        function
        | BinaryOp "Exponentiation" (l,r)-> Some(l, r)  
        | _ -> None

    let addOp = 
        Map.empty
            .Add("UInt16", <@ (+) 0us 0us @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op info for %A" (+))
            .Add("Int16", <@ (+) 0s 0s @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op info for for %A" (+))
            .Add("UInt32", <@ (+) 0u 0u @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op info for %A" (+))
            .Add("Int32", <@ (+) 0 0 @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op info for for %A" (+))
            .Add("UInt64", <@ (+) 0UL 0UL @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op info for %A" (+))
            .Add("Int64", <@ (+) 0L 0L @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op info for %A" (+))
            .Add("Single", <@ (+) 0.0f 0.0f @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op info for %A" (+))
            .Add("Double", <@ (+) 0. 0. @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op info for %A" (+))
            .Add("Decimal", <@ (+) 0m 0m @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op info for %A" (+))
            .Add("Rational", <@ (+) 0Q 0Q @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op info for %A" (+))
  
    let subOp = 
        Map.empty
            .Add("UInt16", <@ (-) 0us 0us @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op infor for %A" (-))
            .Add("Int16", <@ (-) 0s 0s @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op info for for %A" (-))
            .Add("UInt32", <@ (-) 0u 0u @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op infor for %A" (-))
            .Add("Int32", <@ (-) 0 0 @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op info for for %A" (-))
            .Add("UInt64", <@ (-) 0UL 0UL @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op infor for %A" (-))
            .Add("Int64", <@ (-) 0L 0L @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op infor for %A" (-))
            .Add("Single", <@ (-) 0.0f 0.0f @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op infor for %A" (-))
            .Add("Double", <@ (-) 0. 0. @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op infor for %A" (-))
            .Add("Decimal", <@ (-) 0m 0m @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op infor for %A" (-))
            .Add("Rational", <@ (-) 0Q 0Q @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op infor for %A" (-))

    let mulOp = 
        Map.empty
            .Add("UInt16", <@ (*) 0us 0us @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op infor for %A" (*))
            .Add("Int16", <@ (*) 0s 0s @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op info for for %A" (*))
            .Add("UInt32", <@ (*) 0u 0u @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op infor for %A" (*))
            .Add("Int32", <@ (*) 0 0 @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op info for for %A" (*))
            .Add("UInt64", <@ (*) 0UL 0UL @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op infor for %A" (*))
            .Add("Int64", <@ (*) 0L 0L @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op infor for %A" (*))
            .Add("Single", <@ (*) 0.0f 0.0f @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op infor for %A" (*))
            .Add("Double", <@ (*) 0. 0. @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op infor for %A" (*))
            .Add("Decimal", <@ (*) 0m 0m @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op infor for %A" (*))
            .Add("Rational", <@ (*) 0Q 0Q @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op infor for %A" (*))

    let divOp = 
        Map.empty
            .Add("UInt16", <@ (/) 0us 0us @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op info for %A" (/))
            .Add("Int16", <@ (/) 0s 0s @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op info for %A" (/))
            .Add("UInt32", <@ (/) 0u 0u @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op infor for %A" (/))
            .Add("Int32", <@ (/) 0 0 @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op info for for %A" (/))
            .Add("UInt64", <@ (/) 0UL 0UL @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op infor for %A" (/))
            .Add("Int64", <@ (/) 0L 0L @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op infor for %A" (/))
            .Add("Single", <@ (/) 0.0f 0.0f @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op info for %A" (/))
            .Add("Double", <@ (/) 0. 0. @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op info for %A" (/))
            .Add("Decimal", <@ (/) 0m 0m @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op info for %A" (/))
            .Add("Rational", <@ (/) 0Q 0Q @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op info for %A" (/))

    let negateOp = 
        Map.empty
            .Add("Int16", <@ (~-) 0s @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op info for for %A" (-))
            .Add("Int32", <@ (~-) 0 @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op info for for %A" (-))
            .Add("Int64", <@ (~-) 0L @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op info for %A" (-))
            .Add("Single", <@ (~-) 0.0f @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op info for %A" (-))
            .Add("Double", <@ (~-) 0. @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op info for %A" (-))
            .Add("Decimal", <@ (~-) 0. @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op info for %A" (-))
            .Add("Rational", <@ (~-) 0Q @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op info for %A" (-))
        
    let absOp = 
        Map.empty
            .Add("Int16", <@ abs 0s @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op info for %A" (/))
            .Add("Int32", <@ abs 0 @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op info for for %A" (/))
            .Add("Int64", <@ abs 0L @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op infor for %A" (/))
            .Add("Single", <@ abs 0.0f @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op info for %A" (/))
            .Add("Double", <@ abs 0. @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op info for %A" (/))
            .Add("Decimal", <@ abs 0m @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op info for %A" (/))
            .Add("Rational", <@ abs 0Q @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op info for %A" (/))

    let sqrtOp = 
        Map.empty
            .Add("Single", <@ sqrt 0.0f @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op info for %A" (/))
            .Add("Double", <@ sqrt 0. @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op info for %A" (/))
            .Add("Rational", <@ sqrt 0Q @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op info for %A" (/))

    let powOp = 
        Map.empty
            .Add("Single", <@ ( ** ) 0.0f 0.0f @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op info for %A" (pown))
            .Add("Double", <@ ( ** ) 0. 0. @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op info for %A" (pown))
            .Add("Rational", <@ ( ** ) 0Q 0Q @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op info for %A" (pown))
    
    let pownOp = 
        Map.empty
            .Add("UInt16", <@ pown 0us 0 @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op info for %A" (pown))
            .Add("Int16", <@ pown 0s 0 @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op info for %A" (pown))
            .Add("UInt32", <@ pown 0u 0 @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op infor for %A" (pown))
            .Add("Int32", <@ pown 0 0 @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op info for for %A" (pown))
            .Add("UInt64", <@ pown 0UL 0 @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op infor for %A" (pown))
            .Add("Int64", <@ pown 0L 0 @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op infor for %A" (pown))
            .Add("Single", <@ pown 0.0f 0 @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op info for %A" (pown))
            .Add("Double", <@ pown 0. 0 @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op info for %A" (pown))
            .Add("Decimal", <@ pown 0m 0 @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op info for %A" (pown))
            .Add("Rational", <@ pown 0Q 0 @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op info for %A" (pown))

    let sinOp = 
        Map.empty
            .Add("Single", <@ sin 0.0f @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op info for %A" (sin))
            .Add("Double", <@ sin 0. @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op info for %A" (sin))
            .Add("Rational", <@ sin 0Q @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op info for %A" (sin))

    let cosOp = 
        Map.empty
            .Add("Single", <@ cos 0.0f @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op info for %A" (sin))
            .Add("Double", <@ cos 0. @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op info for %A" (sin))
            .Add("Rational", <@ cos 0Q @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op info for %A" (sin))
    
    let logOp = 
        Map.empty
            .Add("Single", <@ log 0.0f @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op info for %A" (log))
            .Add("Double", <@ log 0. @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op info for %A" (log))

    let log10Op = 
        Map.empty
            .Add("Single", <@ log10 0.0f @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op info for %A" (log10))
            .Add("Double", <@ log10 0. @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op info for %A" (log10))
    let minOp = 
           Map.empty
               .Add("UInt16", <@ Math.Min(0us, 0us) @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op info for %A" (pown))
               .Add("Int16", <@ Math.Min(0s, 0s) @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op info for %A" (pown))
               .Add("UInt32", <@ Math.Min(0u, 0u) @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op infor for %A" (pown))
               .Add("Int32", <@ Math.Min(0, 0) @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op info for for %A" (pown))
               .Add("UInt64", <@ Math.Min(0UL, 0UL) @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op infor for %A" (pown))
               .Add("Int64", <@ Math.Min(0L, 0L) @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op infor for %A" (pown))
               .Add("Single", <@ Math.Min(0.0f, 0.0f) @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op info for %A" (pown))
               .Add("Double", <@ Math.Min(0., 0.) @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op info for %A" (pown))
               .Add("Decimal", <@ Math.Min(0m, 0m) @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op info for %A" (pown))
               //.Add("Rational", <@ Math.Min(0Q, 0) @> |> function |Call(_, mi, _) -> mi | _ -> failwithf "Could not get op info for %A" (pown))


    let zeroVal = 
        Map.empty
            .Add("UInt16", <@ (+) 0us 0us @> |> function |Call(_, _, l::_) -> l | _ -> failwithf "Could not get info for zerp.")
            .Add("Int16", <@ (+) 0s 0s @> |> function |Call(_, _, l::_) -> l | _ -> failwithf "Could not get info for zerp.")
            .Add("UInt32", <@ (+) 0u 0u @> |> function |Call(_, _, l::_) -> l | _ -> failwithf "Could not get info for zerp.")
            .Add("Int32", <@ (+) 0 0 @> |> function |Call(_, _, l::_) -> l | _ -> failwithf "Could not get info for zerp.")
            .Add("UInt64", <@ (+) 0UL 0UL @> |> function |Call(_, _, l::_) -> l | _ -> failwithf "Could not get info for zerp.")
            .Add("Int64", <@ (+) 0L 0L @> |> function |Call(_, _, l::_) -> l | _ -> failwithf "Could not get info for zerp.")
            .Add("Single", <@ (+) 0.0f 0.0f @> |> function |Call(_, _, l::_) -> l | _ -> failwithf "Could not get info for zerp.")
            .Add("Double", <@ (+) 0. 0. @> |> function |Call(_, _, l::_) -> l | _ -> failwithf "Could not get info for zerp.")
            .Add("Decimal", <@ (+) 0m 0m @> |> function |Call(_, _, l::_) -> l | _ -> failwithf "Could not get info for zerp.")
            .Add("Rational", <@ (+) 0Q 0Q @> |> function |Call(_, _, l::_) -> l | _ -> failwithf "Could not get info for zerp.")

    let oneVal = 
        Map.empty
            .Add("UInt16", <@ (+) 1us 1us @> |> function |Call(_, _, l::_) -> l | _ -> failwithf "Could not get info for one.")
            .Add("Int16", <@ (+) 1s 1s @> |> function |Call(_, _, l::_) -> l | _ -> failwithf "Could not get info for one.")
            .Add("UInt32", <@ (+) 1u 1u @> |> function |Call(_, _, l::_) -> l | _ -> failwithf "Could not get info for one.")
            .Add("Int32", <@ (+) 1 1 @> |> function |Call(_, _, l::_) -> l | _ -> failwithf "Could not get info for one.")
            .Add("UInt64", <@ (+) 1UL 1UL @> |> function |Call(_, _, l::_) -> l | _ -> failwithf "Could not get info for one.")
            .Add("Int64", <@ (+) 1L 1L @> |> function |Call(_, _, l::_) -> l | _ -> failwithf "Could not get info for one.")
            .Add("Single", <@ (+) 1.0f 1.0f @> |> function |Call(_, _, l::_) -> l | _ -> failwithf "Could not get info for one.")
            .Add("Double", <@ (+) 1. 1. @> |> function |Call(_, _, l::_) -> l | _ -> failwithf "Could not get info for one.")
            .Add("Decimal", <@ (+) 1m 1m @> |> function |Call(_, _, l::_) -> l | _ -> failwithf "Could not get info for one.")
            .Add("Rational", <@ (+) 1Q 1Q @> |> function |Call(_, _, l::_) -> l | _ -> failwithf "Could not get info for one.")

    let negOneVal = 
        Map.empty
            .Add("Int16", <@ (+) -1s 1s @> |> function |Call(_, _, l::_) -> l | _ -> failwithf "Could not get info for one.")
            .Add("Int32", <@ (+) -1 1 @> |> function |Call(_, _, l::_) -> l | _ -> failwithf "Could not get info for one.")
            .Add("Int64", <@ (+) -1L 1L @> |> function |Call(_, _, l::_) -> l | _ -> failwithf "Could not get info for one.")
            .Add("Single", <@ (+) -1.0f 1.0f @> |> function |Call(_, _, l::_) -> l | _ -> failwithf "Could not get info for one.")
            .Add("Double", <@ (+) -1. 1. @> |> function |Call(_, _, l::_) -> l | _ -> failwithf "Could not get info for one.")
            .Add("Decimal", <@ (+) -1m 1m @> |> function |Call(_, _, l::_) -> l | _ -> failwithf "Could not get info for one.")
            .Add("Rational", <@ (+) -1Q 1Q @> |> function |Call(_, _, l::_) -> l | _ -> failwithf "Could not get info for one.")
    
    let rec getExprName = function
        | Call(None, info, _) -> info.Name
        | Lambda(_, expr) -> getExprName expr
        | PropertyGet (_, info, _) -> info.Name
        | FieldGet (_, info) -> info.Name
        | _ -> failwith "Expression does not have information to retrieve name."

    let getExprFromReflectedDefinition<'t> =
        function
        | WithValue(v, t, _) when t = typeof<'t> -> v :?> 't
        | ValueWithName(v, t, _) when t = typeof<'t> -> v :?> 't
        | expr -> failwithf "%s is not a reflected definition of type %s. It is a %s with type %s." (src expr) (typeof<'t>.Name) (expr.ToString()) (expr.Type.Name)
        
    let getExprFromReflectedDefinition'<'t> =
        function
        | WithValue(v, t, _) when t = typeof<'t> -> "", v :?> 't
        | ValueWithName(v, t, n) when t = typeof<'t> -> n, v :?> 't
        | expr -> failwithf "%s is not a reflected definition of type %s." (src expr) (typeof<'t>.Name)

    let getVal<'t> =
        function
        | Value(v, t) when t = typeof<'t> -> v :?> 't
        | expr -> failwithf "The expression %A has type %s and is not a value of type %s." (expr) (expr.Type.Name) (typeof<'t>.Name)

    let getVal'<'t> =
        function
        | Value(v, t) when t = typeof<'t> -> v :?> 't
        | ValueWithName(v, t, _) when t = typeof<'t> -> v :?> 't
        | WithValue(v, t, _ ) when t = typeof<'t> -> v :?> 't
        | Var _ -> Unchecked.defaultof<'t>
        | expr -> failwithf "The expression %A has type %s and is not like a value of type %s." (expr) (expr.Type.Name) (typeof<'t>.Name)

    let getVal''<'t> =
        function
        | Value(v, t) when t = typeof<'t> -> v :?> 't
        | ValueWithName(v, t, _) when t = typeof<'t> -> v :?> 't
        | WithValue(v, t, _ ) when t = typeof<'t> -> v :?> 't
        | e when e.Type = typeof<'t> -> Unchecked.defaultof<'t>
        | expr -> failwithf "The expression %A has type %s and is not a value or expression of type %s." (expr) (expr.Type.Name) (typeof<'t>.Name)

    let hasCase<'t> case = FSharpType.GetUnionCases(typeof<'t>) |> Array.tryFind(fun c -> c.Name = case)
    
    let sequal (l:Expr) (r:Expr) = 
        (l.ToString() = r.ToString()) 
        || l.ToString() = sprintf "(%s)" (r.ToString())
        || r.ToString() = sprintf "(%s)" (l.ToString())
    
    let sequal2 (l1:Expr) (l2:Expr) (r1:Expr) (r2:Expr) = sequal l1 r1 && sequal l2 r2
    
    let sequal3 (l1:Expr) (l2:Expr) (l3:Expr) (r1:Expr) (r2:Expr) (r3:Expr)= sequal l1 r1 && sequal l2 r2 && sequal l3 r3

    let vequal (lv1:Var) (lv2:Var) = lv1.Name = lv2.Name && lv1.Type = lv2.Type

    let vequal2 (lv1:Var) (lv2:Var) (rv1:Var) (rv2:Var) = vequal lv1 rv1 && vequal lv2 rv2
    
    let vequal3 (lv1:Var) (lv2:Var) (lv3:Var) (rv1:Var) (rv2:Var) (rv3:Var) = vequal lv1 rv1 && vequal lv2 rv2 && vequal lv3 rv3

    let vequal_single (lv1:Var) (lv2:Var list) = lv2.Length = 1 && vequal lv2.Head lv1

    let vequal' (lv1:Var list) (lv2:Var list) = lv1.Length = lv2.Length && List.fold2(fun s v1 v2 -> s && vequal v1 v2) true lv1 lv2

    let rec body = 
        function
        | Lambda(_, (Lambda(_, _) as l)) -> body l
        | Lambda(_, (Call(_, f, _) as b))  -> match Expr.TryGetReflectedDefinition f with | Some e -> body e | None -> b
        | Lambda(_, b) -> b
        | Let(_, _, b) -> b
        | expr -> failwithf "The expression %A is not a function or a let binding." expr

    let rec param_vars : Expr->Var list = 
         function
         | Lambda(v, (Lambda(_, _) as l))  -> [v] @ (param_vars l)
         | Lambda(v, _) -> [v]
         | Let(v, _, _) -> [v]
         | expr -> failwithf "The expression %A is not a function or a let binding." expr
        
    let rec recombine_func (vars:Var list) (body:Expr) =
        match vars with
        | [] -> failwithf "Cannot recombine function body %A with an empty parameter list." body
        | v::[] -> Expr.Lambda(v, body)
        | v1::v2::[] -> Expr.Lambda(v1, Expr.Lambda(v2, body))
        | v -> recombine_func (List.take (v.Length - 1) v) (Expr.Lambda(List.last v, body))

    let traverse expr f =
        match expr with
        | ShapeVar v -> Expr.Var v
        | ShapeLambda (v, body) -> Expr.Lambda (v, f body)
        | ShapeCombination (o, exprs) -> RebuildShapeCombination (o,List.map f exprs)

    /// Based on: http://www.fssnip.net/1i/title/Traverse-quotation by Tomas Petrick
    /// Traverse an entire quotation and use the provided function
    /// to transform some parts of the quotation. If the function 'f'
    /// returns 'Some' for some sub-quotation then we replace that
    /// part of the quotation. The function then recursively processes
    /// the quotation tree.
    let rec traverse' f q = 
      let q = defaultArg (f q) q
      match q with
      | ExprShape.ShapeCombination(a, args) -> 
          let nargs = args |> List.map (traverse' f)
          ExprShape.RebuildShapeCombination(a, nargs)
      | ExprShape.ShapeLambda(v, body)  -> 
          Expr.Lambda(v, traverse' f body)
      | ExprShape.ShapeVar(v) ->
          Expr.Var(v)


    let subst_var_value (var:Var) (value: Expr) (expr:Expr)  =
        match expr with
        | ShapeLambda(bound, body) -> Expr.Lambda(bound, body.Substitute(fun v -> if v.Name = var.Name && v.Type = var.Type then Some value else None))
        | Application(a, x) -> 
            let a' = a.Substitute(fun v -> if v.Name = var.Name && v.Type = var.Type then Some value else None) in
                Expr.Application(a', x)
        | expr -> expr.Substitute(fun v -> if v.Name = var.Name && v.Type = var.Type then Some value else None)

    let subst_var_value' (lvar:Var list) (lval:Expr list) (expr:Expr)  =
        do if not (lvar.Length = lval.Length) then failwithf "The lengths of the variable/expression lists lists are not the same for replacement operation in %s." (src expr)
        List.fold2 (fun e v s -> subst_var_value v s e) expr lvar lval

    let subst_all_value (expr:Expr) (value:Expr) = expr.Substitute(fun _ -> Some value)
    
    let rec replace_var_expr (var:Var) (value:Expr) (expr:Expr)  =
        match expr with
        | ShapeVar v  when vequal v var ->  value
        | expr -> traverse expr (replace_var_expr var value)

    let replace_var_expr' (lvar:Var list) (lexpr:Expr list) (expr:Expr)  =
        do if not (lvar.Length = lexpr.Length) then failwithf "The lengths of the variable/expression lists lists are not the same for replacement operation in %s." (src expr)
        List.fold2 (fun e v r -> replace_var_expr v r e) expr lvar lexpr

    let rec replace_var_type<'t> =
        function
        | ShapeVar v when v.Type <> typeof<'t> -> Expr.Var(Var(v.Name, typeof<'t>))
        | expr -> traverse expr replace_var_type<'t>

    let rec replace_var_var (var1:Var) (var2:Var) (expr:Expr)  =
        match expr with
        | ShapeVar v  when vequal v var1-> Expr.Var var2
        | expr -> traverse expr (replace_var_var var1 var2)

    let replace_var_var' (lvar1:Var list) (lvar2:Var list) (expr:Expr)  =
        do if not (lvar1.Length = lvar2.Length) then failwithf "The lengths of the variable lists are not the same for replacement operation in %s." (src expr)
        List.fold2 (fun e v1 v2 -> replace_var_var v1 v2 e) expr lvar1 lvar2
       
        
    let rec replace_expr (o:Expr) (n:Expr)  = 
            function
            | l when sequal l o && l.Type = o.Type -> n
            | expr -> traverse expr (replace_expr o n)
            
    let get_vars expr =
        let rec rget_vars prev expr =
            match expr with
            | PropertyGet(None, _, []) -> prev @ []
            | Call(_, _, exprs) -> List.map (rget_vars prev) exprs |> List.collect id
            | ShapeVar v -> prev @ [v]
            | ShapeLambda (v, body) -> rget_vars (prev @ [v]) body
            | ShapeCombination (_, exprs) ->  List.map (rget_vars prev) exprs |> List.collect id
            
        rget_vars [] expr |> List.distinctBy (fun v -> v.Name)

    let get_varsl<'t> (exprs: Expr<'t> list) =
        let vars = List<Var>()
        exprs |> List.iter(fun e -> e |> get_vars|> List.iter(fun v -> if not <| vars.Any(fun _v -> vequal v _v) then vars.Add v))
        vars |> Seq.toList

    
    let get_varss<'t> (exprs: Expr<'t> seq) =
        let vars = List<Var>()
        exprs |> Seq.iter(fun e -> e |> get_vars|> List.iter(fun v -> if not <| vars.Any(fun _v -> vequal v _v) then vars.Add v))
        vars :> seq<Var>

    let get_var expr = get_vars expr |> Seq.exactlyOne

    let get_var_names expr = get_vars expr |> List.map (fun v -> v.Name)
    
    let vname(v:Var) = v.Name

    let has_var (v:Var) (vars:Var list) = vars |> List.tryFind(fun vf -> vf.Name = v.Name && vf.Type = v.Type) |> Option.isSome
    
    let get_var_count expr = expr |> get_vars |> List.length

    let fail_if_not_has_var (v:Var) (expr:Expr) = 
        let vars = get_vars expr
        do if not <| has_var v vars then failwithf "The expression %A does not contain the variable %A." (src expr) v

    let occurs (var:Var list) (expr:Expr) = 
        expr |> get_vars |> List.exists(fun v -> var |> List.exists(fun vv -> vequal v vv))

    let not_occurs (var:Var list) (expr:Expr) = not (occurs var expr)

    let vars_to_tuple (vars:Var list) = 
           match vars with
           | v::[] -> Expr.Var v
           | v::[] when FSharpType.IsTuple v.Type -> Expr.Var v
           | _ -> vars |> List.map (fun v -> Expr.Var v) |> Expr.NewTuple

    let get_vars_to_tuple (x:Expr) = x |> get_vars |> vars_to_tuple 
            
    let rec (|List|_|) =
        let isListType (u:UnionCaseInfo) = u.DeclaringType.IsGenericType && u.DeclaringType.GetGenericTypeDefinition() = typedefof<list<_>>
        function
        | NewUnionCase(uci, []) when isListType uci -> Some []
        | NewUnionCase(uci, lhs::(NewUnionCase(_, []))::[]) when isListType uci  -> Some (lhs::[])
        | NewUnionCase(uci, lhs::List(rhs)::[]) when isListType uci  -> Some (lhs::rhs)
        | _ -> None

    let (|WithoutVariables|_|):Expr->Expr option =
        function
        | e when (get_vars e) = List.empty -> Some e
        | _ -> None

    /// Based on: http://www.fssnip.net/bx/title/Expanding-quotations by Tomas Petricek.
    /// Expand variables and calls to methods and property getters.
    let expand expr =
        let rec rexpand vars expr = 
          let expanded = 
            match expr with
            | WithValue(_, _, e) -> rexpand vars e
            //| ValueWithName(o,t,_) -> Expr.Value(o, t)
            | SpecificCall <@@ List.toArray @@>(None,t::[], l::[]) -> 
                match l with
                | List el -> rexpand vars (Expr.NewArray(t, el))
                | WithValue(_, _, List el) -> rexpand vars (Expr.NewArray(t, el))
                | ValueWithName(o, t, _) -> Expr.Value(o, t)
                | e -> failwithf "Unknown expression trying to expand List.toArray: %A." e
            | Call(None, Op "FromInt32" ,Value(v, _)::[]) as e when e.Type = typeof<Rational> -> Expr.Value(Rational((v :?> int32), 1))
            | Call(None, Op "ToDouble" ,Value(v, _)::[]) as e when e.Type = typeof<real> -> Expr.Value(Convert.ToDouble(v))
            | Call (None, Op "FromZero", _) as e -> e
            | Call(body, MethodWithReflectedDefinition meth, args) ->
                let this = match body with Some b -> Expr.Application(meth, b) | _ -> meth
                let res = Expr.Applications(this, [ for a in args -> [a]])
                rexpand vars res
            | PropertyGet(body, PropertyGetterWithReflectedDefinition p, []) -> 
                let this = match body with Some b -> b | None -> p
                rexpand vars this
            //| PropertyGet(None, p, []) -> rexpand vars (Expr.Var(Var(p.Name, p.PropertyType)))
            // If the variable has an assignment, then replace it with the expression
            | ExprShape.ShapeVar v when Map.containsKey v vars -> vars.[v]
            // Else apply rexpand recursively on all sub-expressions
            | _ -> traverse expr (rexpand vars)
          // After expanding, try reducing the expression - we can replace 'let' expressions and applications where the first argument is lambda.
          match expanded with
          | Application(ExprShape.ShapeLambda(v, body), assign)
          | Let(v, assign, body) ->
                rexpand (Map.add v (rexpand vars assign) vars) body
          | _ -> expanded

        rexpand Map.empty expr

    // Expand typed expression and cast to another type.
    let expand_cast<'a, 'b> (expr:Expr<'b>) =
        let e = expand expr in <@ %%e:'a @>

    // Expand untyped expression and cast to type.
    let expand_as<'t> (expr:Expr) =
        let e = expand expr in <@ %%e:'t @>

    let expand_left = 
        function
        | Call(_,_,l::r::[]) when l.Type = r.Type -> expand l 
        | expr -> failwithf "%s is not a binary expression." (src expr)

    let expand_right = 
        function
        | Call(_,_,l::r::[]) when l.Type = r.Type -> expand r
        | expr -> failwithf "%s is not a binary expression." (src expr)

    let expandReflectedDefinitionParam = 
        function
        | WithValue(v, t, e) -> (v, t, expand e)
        | _ -> failwith "Expression is not a reflected definition parameter."

    let exprv<'t>(v:'t) = v |> Expr.Value |> expand_as<'t>

    let exprvar<'t>(v:Var) = v |> Expr.Var |> expand_as<'t>

    let expr_var<'t>(n:string) = expand_as<'t>(Expr.Var(Var(n, typeof<'t>)))

    let body'(expr:Expr<'a->'b>) = expr |> body |> expand_as<'b>
    
    let binary_operands op (e:Expr) =
           match e with
             |SpecificCall op (None,_,l::r::[]) -> l, r
             | _ -> failwithf "The expression %s is not a binary %s operation." (src e) (src op)

    let call expr p = 
        let mi = getFuncInfo expr
        Expr.Call(mi, p)

    let binary_call (so, m, l, r) =
        match so with
        | None -> Expr.Call(m, l::r::[])
        | Some o -> Expr.Call(o, m, l::r::[])

    let unary_call (so, m, l) =
        match so with
        | None -> Expr.Call(m, [l])
        | Some o -> Expr.Call(o, m, [l])

    let call_add (l:Expr) (r:Expr) = if addOp.ContainsKey l.Type.Name then binary_call(None, addOp.[l.Type.Name], l, r) else failwithf "The type %A does not support the addition operator." l.Type.Name

    let call_mul (l:Expr) (r:Expr) = binary_call(None, mulOp.[l.Type.Name], l, r)

    let call_sub (l:Expr) (r:Expr) = binary_call(None, subOp.[l.Type.Name], l, r)

    let call_div (l:Expr) (r:Expr) = binary_call(None, divOp.[l.Type.Name], l, r)

    let call_abs (r:Expr) = unary_call(None, absOp.[r.Type.Name], r)

    let call_neg (r:Expr) = unary_call(None, negateOp.[r.Type.Name], r)

    let call_sqrt (r:Expr) = unary_call(None, sqrtOp.[r.Type.Name], r)

    let call_pow (l:Expr) (r:Expr) = binary_call(None, powOp.[l.Type.Name], l, r)

    let call_sin (l:Expr) = unary_call(None, sinOp.[l.Type.Name], l)

    let call_cos (l:Expr) = unary_call(None, cosOp.[l.Type.Name], l)

    let call_log (l:Expr) = unary_call(None, logOp.[l.Type.Name], l)
    
    let call_pown (l:Expr) (r:Expr) = binary_call(None, pownOp.[l.Type.Name], l, r)

    let call_min (l:Expr) (r:Expr) = binary_call(None, minOp.[l.Type.Name], l, r)

    let zero_val(t:Type) = zeroVal.[t.Name]
    
    let one_val(t:Type) = oneVal.[t.Name]

    let neg_one_val(t:Type) = negOneVal.[t.Name]

    let krdelta<'t>  = fun (i:int) (j:int) -> if i = j then one_val (typeof<'t>) else zero_val (typeof<'t>)

    let expand_list (expr:Expr): Expr list =
        let expr' = expand expr
        match expr' with
        | List l -> l 
        | _ -> failwithf "The expression %A is not a list expression." expr'

    let expand_list'(expr: Expr<'t list>) =
        let expr' = expand expr
        match expr' with
        | Patterns.Value(o, _) -> let l = o :?> 't list in l |> List.map(fun ee -> <@ ee @>) 
        | List l -> l |> List.map expand |> List.map(fun ee -> <@ %%ee:'t @>)
        | ue -> failwithf "Unknown list expression %A." ue

    let expand_lists (expr: Expr<'t list list>) = expr |> expand_list |> List.map expand_list

    let expand_lists' (expr: Expr<'t list list>) = expr |> expand_list' |> List.map expand_list'

    let expand_tuple<'t> (expr:Expr) =
        match expr with
        | NewTuple el when el |> List.forall(fun e -> e.Type = typeof<'t>) -> Some (List.map expand_as<'t> el)
        | _ -> None
    
    let expand_tuples<'t> (expr:Expr) =
        match expr with
        | NewTuple el when el |> List.forall(fun e -> e.Type |> FSharpType.IsTuple && e.Type |> FSharpType.GetTupleElements |> Array.forall(fun t -> t = typeof<'t>)) -> 
            el |> List.map (expand_tuple<'t> >> Option.get) |> Some
        | _ -> None
    
    let expand_equality =
        function
        | SpecificCall <@@ ( = ) @@> (_, _, [l; r]) -> expand l, expand r
        | expr -> failwithf "The expression %s is not a equality expression." <| src expr

    let param_var (f:Expr<'a->'b>) = f |> param_vars |> List.exactlyOne 

    let param_var_expr (f:Expr<'a->'b>) = f |> param_vars |> List.exactlyOne |> Expr.Var |> expand_as<'a>

    let subst_func_var_value (f:Expr<'a->'b>) (r:Expr) =            
        let v = param_var f
        f |> body |> subst_var_value v r |> recombine_func [v] |> expand_as<'a->'b>
            
    let evaluate (q:Expr<'t>) = 
        match q with
        | Var _ -> Unchecked.defaultof<'t>
        | _ -> FSharp.Quotations.Evaluator.QuotationEvaluator.Evaluate q

    let ev q  = evaluate q

    let as_func_of (v:Expr<'t>) (body:Expr<'u>) =
        body |> recombine_func (get_vars v) |> expand_as<'t->'u> |> ev

    let as_func_of2 (x:Expr<'t>) (y:Expr<'u>) (body:Expr<'v>) =
        body |> recombine_func (get_vars x @ get_vars y) |> expand_as<'t->'u->'v> |> ev

    let as_func_of_single_var<'t> (expr:Expr<'t>) =
        let v = get_vars expr
        match v with
        | [] -> let x = Var("x", typeof<'t>) in recombine_func [x] expr |> expand_as<'t->'t> |> ev 
        | x::[] -> recombine_func [x] expr |> expand_as<'t->'t> |> ev
        | _ -> failwithf "Expression %A is not an expression of a single variable." expr

    let is_inst_expr (bv:Var) (l:Expr) (r:Expr)=
        let s = src l
        let s' = src r
        let m = l.Substitute(fun v -> if vequal v bv then Expr.Var(new Var("$$_$$", l.Type)) |> Some else None)
        let p = (src m).IndexOf("$$_$$")
        if p = -1 || p > s'.Length - 1 then
            sequal l r
            
        else 
            let v = s' |> Seq.skip p |> Seq.takeWhile(fun c -> c <> ' ') |> Seq.toArray |> String
            let s'' = (src m).Replace("$$_$$", v)
            s' = s''

    let find_expr s t expr =
        let dict = new System.Collections.Generic.List<Expr>()
        expr |> traverse' (fun q -> 
        if src q = s && q.Type = t then dict.Add q
        None) |> ignore
        dict |> List.ofSeq

    let recombine_func_as<'t> vars body = recombine_func vars body |> expand_as<'t>

    let has_prop<'t> n (o:obj) = o.GetType().GetProperties().Any(fun p -> p.Name = n && p.PropertyType = typeof<'t>)
    
    let get_prop<'t> n (o:obj) = o.GetType().GetProperties().First(fun p -> p.Name = n && p.PropertyType = typeof<'t>).GetValue(o) :?> 't

    let get_prop_else<'t> n (e:'t) (o:obj) = if has_prop<'t> n o then o.GetType().GetProperties().First(fun p -> p.Name = n && p.PropertyType = typeof<'t>).GetValue(o) :?> 't else e
    
    let get_prop_or_fail<'t> (p:string) (f:string) (attrs:obj) =
       if has_prop<'t> p attrs then get_prop<'t> p attrs else failwith f

    let has_dict_prop<'t> n (o:IDictionary<string, obj>) = o.Any(fun p -> p.Key = n && p.Value.GetType() = typeof<'t>)
       
    let get_dict_prop<'t> n (o:IDictionary<string, obj>) = o.First(fun p -> p.Key = n && p.Value.GetType()= typeof<'t>).Value :?> 't

    let get_dict_prop_else<'t> n (e:'t) (o:IDictionary<string, obj>) = if has_dict_prop<'t> n o then get_dict_prop<'t> n o else e
       
    let to_dict (o:obj) = 
        let dict = new Dictionary<string, obj>()
        for p in o.GetType().GetProperties() do
            let k = p.Name
            let v = p.GetValue(o)
            if not (dict.ContainsKey k) then dict.Add(k, v) else dict.[k] <- v
        dict

    let merge_dict(dict:IDictionary<string, obj>) (o:obj) =
        for p in o.GetType().GetProperties() do
            let k = p.Name
            let v = p.GetValue(o)
            if not (dict.ContainsKey k) then dict.[k] <- v
        dict

    let merge_overwrite_dict(dict:IDictionary<string, obj>) (o:obj) =
           for p in o.GetType().GetProperties() do
               let k = p.Name
               let v = p.GetValue(o)
               dict.[k] <- v
           dict

    let get_consts expr =
            let dict = new System.Collections.Generic.List<Type*string>()
            expr |> traverse' (function | ValueWithName(_, t, n) -> dict.Add(t, n); None | _ -> None) |> ignore
            dict |> List.ofSeq

    let get_symbols e = get_vars e |> List.map(fun v -> v.Type, v.Name) |> List.append(get_consts e)

    let nullv = <@ %%Expr.Value(null, typeof<obj>):obj @>

    let exprs_all_numeric(s:seq<Expr<_>>) = s |> Seq.tryFind(function | Value _ -> false | _ -> true) |> Option.isSome
    
    let end_with = ignore

    let head (l:seq<_>) = Seq.head l

    let tail (l:seq<_>) = Seq.tail l

    let symbolic_fn<'t> (sym:string) (v: string array) = Unchecked.defaultof<'t>

    let is_symbolic_fn = 
        function    
        |  SpecificCall <@@ symbolic_fn @@> (_,_,[String _;NewArray(_, _)]) -> true
        | _ -> false

    let rec (|LinearExpr|_|) (x:string) (expr:Expr) =
           let t = expr.Type in
           match expr with
           | Constant _ -> Some expr    
           | Variable (Var v) when v.Name = x -> Some expr
           | Multiplication(Constant _, Constant _) -> Some expr
           | Multiplication(Constant _, Variable(Var v)) when v.Name = x -> Some expr
           | Multiplication(Variable(Var v), Constant _)-> Some expr
           | Addition(LinearExpr x _, LinearExpr x _) -> Some expr
           | Subtraction(LinearExpr x l, LinearExpr x r) -> Some <| call_add l (call_mul (neg_one_val(t)) r)
           | _ -> None
       
    let rec (|LinearTerms|_|) (expr:Expr) =
        let t = expr.Type in
        match expr with
        | Constant _ -> [[expr]] |> Some
        | Variable _ -> [[expr]] |> Some 
        | Multiplication(Constant c1, Constant  c2) ->[[call_mul c1 c2]] |> Some
        | Multiplication(Constant c, Variable v)
        | Multiplication(Variable v, Constant c)-> [[c;v]] |> Some
        | Addition(LinearTerms l, LinearTerms r) -> l @ r |> Some
        | Subtraction(LinearTerms l, LinearTerms r) -> l @ (List.map(List.map (call_mul (neg_one_val t))) r) |> Some
        | _ -> None

    let rec (|LinearTermsOf|_|) x (expr:Expr) =
        let t = expr.Type in
        match expr with
        | Constant _ -> [[expr]] |> Some
        | VariableWithOneOfNames x _ -> [[expr]] |> Some 
        | Multiplication(Constant c1, Constant  c2) ->[[call_mul c1 c2]] |> Some
        | Multiplication(Constant c, VariableWithOneOfNames x v)
        | Multiplication(VariableWithOneOfNames x v, Constant c)-> [[c;v]] |> Some
        | Addition(LinearTermsOf x l, LinearTermsOf x r) -> l @ r |> Some
        | Subtraction(LinearTermsOf x l, LinearTermsOf x r) -> l @ (List.map(List.map (call_mul (neg_one_val t))) r) |> Some
        | _ -> None

    let rec (|LinearCoeff|_|) (expr:Expr) =
           let t = expr.Type in
           match expr with
           | Constant c -> Some [c,c] 
           | Addition(Constant c1, Constant c2) -> let c = call_add c1 c2 in Some [c,c]
           | Multiplication(Constant c1, Constant c2) -> Some [c1,c2]
           | Variable v -> Some [one_val t, v]  
           | Multiplication(Constant c, Variable v)
           | Multiplication(Variable v, Constant c)-> Some [(c,v)]
           | Multiplication(LinearCoeff [Constant c1, Constant c2], LinearCoeff [Constant c3, (Var _ as v)]) -> if sequal c1 c2 then Some [call_mul c1 c3, v] else [call_mul (call_mul c1 c2) c3, v] |> Some
           | Multiplication(LinearCoeff [Constant c1, (Var _ as v)], LinearCoeff [Constant c2, Constant c3]) -> if sequal c2 c3 then Some [call_mul c1 c3, v] else [call_mul (call_mul c2 c3) c1, v] |> Some
           | Addition(LinearCoeff ([c1,v1]), LinearCoeff ([c2,v2])) when sequal v1 v2 -> [call_add c1 c2, v1] |> Some
           | Addition(LinearCoeff l, LinearCoeff r) -> l @ r |> Some
           | Subtraction(LinearCoeff ([(c1,v1)]), LinearCoeff ([(c2,v2)])) when sequal v1 v2 -> [call_add c1 (call_mul (neg_one_val t) c2), v1] |> Some
           | Subtraction(LinearCoeff l, LinearCoeff r) -> l @ (List.map (fun (c,v) ->  (call_mul (neg_one_val t) c,v)) r) |> Some
           | _ -> None

    