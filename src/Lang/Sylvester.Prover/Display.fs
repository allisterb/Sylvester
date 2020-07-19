namespace Sylvester

open System.Reflection
open FSharp.Quotations
open FSharp.Quotations.DerivedPatterns
open FSharp.Reflection
open Patterns
    
module Display = 
    let mutable transliterateGreekSymbols = true

    let (|SymbolDisplay|_|):obj -> string option = 
        function
        | :? MethodInfo as info when Symbols.BulitIn.ContainsKey info.Name -> Symbols.BulitIn.[info.Name] |> Some
        | :? MethodInfo as info when info.GetCustomAttributes(typeof<SymbolAttribute>, true) <> null ->
            let a =  info.GetCustomAttributes(typeof<SymbolAttribute>, true) in
            let u = (a.[0] :?> SymbolAttribute) in u.Symbol |> Some 
        | :? PropertyInfo as info when Symbols.BulitIn.ContainsKey info.Name -> Symbols.BulitIn.[info.Name] |> Some
        | :? UnionCaseInfo as info when Symbols.BulitIn.ContainsKey info.Name -> Symbols.BulitIn.[info.Name] |> Some
        | :? System.Type as t ->
            let a = t.GetCustomAttributes(typeof<SymbolAttribute>, true) in
            if a = null || a.Length = 0 then None else let u = (a.[0] :?> SymbolAttribute) in u.Symbol |> Some 
        //| :? Expr as expr when Symbols.BulitIn.ContainsKey (expr |> expand |> src) -> Symbols.BulitIn.[(expr |> expand |> src)] |> Some
        | :? string as s when Symbols.Greek.ContainsKey s && transliterateGreekSymbols -> Symbols.Greek.[s] |> Some
        | :? string as s -> s |> Some
        | _ -> None

    let (|VariableDisplay|_|):obj -> string option =
        function
        | :? Var as v -> v.Name |> Some
        | :? (Var list) as vars -> vars.Tail |> List.fold (fun s v -> sprintf "%s,%s" s v.Name) vars.Head.Name |> Some
        | _ -> None

    let rec print_formula = 
        function
        (* Binary and unary operators *)
        | Equals(l, r) -> sprintf "%s = %s" (print_formula l) (print_formula r)
        | BinaryFormula(SymbolDisplay symbol, l, r) -> 
            match l, r with
            | Var _, Var _ 
            | Quantifier _, Var _
            | Var _, Quantifier _ 
            | Quantifier _, Quantifier _
            | _, Not _ 
            | Not _, _ -> sprintf "%s %s %s" (print_formula l) (symbol) (print_formula r)
            | Var _, _ -> sprintf "%s %s (%s)" (print_formula l) (symbol) (print_formula r)
            | _, Var _ -> sprintf "(%s) %s %s" (print_formula l) (symbol) (print_formula r)
            | _ -> sprintf "%s %s %s" (print_formula l) (symbol) (print_formula r)
        | UnaryFormula(SymbolDisplay symbol , r) -> 
            match r with
            | Var _ 
            | Quantifier _ -> sprintf "%s%s" (symbol) (print_formula r)
            | _ -> sprintf "%s(%s)" (symbol) (print_formula r)

        (* Logical quantifiers *)
        | ForAll(_, VariableDisplay v, Bool true, body) -> sprintf "(\u2200 %s |: %s)" v (print_formula body)
        | ForAll(_, VariableDisplay v, range, body) -> sprintf "(\u2200 %s | %s : %s)" v (print_formula range) (print_formula body)
        | Exists(_, VariableDisplay v, Bool true, body) -> sprintf "(\u2203 %s | %s)" v (print_formula body)
        | Exists(_, VariableDisplay v, range, body) -> sprintf "(\u2203 %s | %s : %s)" v (print_formula range) (print_formula body)
        
        (* Quantifiers*)
        | SumFormula(_, SymbolDisplay symbol, VariableDisplay bound, range, body) -> sprintf "%s %s %s" symbol (bound) (print_formula body)
        
        (* Values and properties*)
        | ValueWithName(_, _, SymbolDisplay symbol) -> symbol
        | NewUnionCase(SymbolDisplay symbol, _) -> symbol
        | Call(None, SymbolDisplay symbol, []) -> symbol
        | PropertyGet(None, SymbolDisplay symbol, []) -> symbol

        (* F# expression source for anything else *)
        | expr -> src expr
