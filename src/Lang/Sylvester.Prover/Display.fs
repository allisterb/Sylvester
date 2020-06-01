namespace Sylvester

open System.Reflection
open FSharp.Quotations
open FSharp.Quotations.DerivedPatterns
open Patterns
    
module Display = 
    let mutable transliterateGreekSymbols = true

    let (|SymbolDisplay|_|):obj -> string option = 
        function
        | :? MethodInfo as info when Symbols.BuiltIn.ContainsKey info.Name -> Symbols.BuiltIn.[info.Name] |> Some
        | :? MethodInfo as info ->
            let a = info.GetCustomAttributes(typeof<SymbolAttribute>, true) in
            if a = null || a.Length = 0 then None else let u = (a.[0] :?> SymbolAttribute) in u.Symbol |> Some 
        | :? System.Type as t ->
            let a = t.GetCustomAttributes(typeof<SymbolAttribute>, true) in
            if a = null || a.Length = 0 then None else let u = (a.[0] :?> SymbolAttribute) in u.Symbol |> Some 
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
        | ForAll(_, VariableDisplay v, Bool true, body) -> sprintf "(\u2200 %s |: %s)" v (body |> print_formula)
        | ForAll(_, VariableDisplay v, range, body) -> sprintf "(\u2200 %s | %s : %s)" v (range |> print_formula) (body |> print_formula)
        | Exists(_, VariableDisplay v, Bool true, body) -> sprintf "(\u2203 %s | %s)" v (body |> print_formula)
        | Exists(_, VariableDisplay v, range, body) -> sprintf "(\u2203 %s | %s : %s)" v (range |> print_formula) (body |> print_formula)
        | UnaryFormula(SymbolDisplay symbol , r) -> sprintf "%s%s" (symbol) (print_formula r)
        | BinaryFormula(SymbolDisplay symbol, l, r) -> sprintf "%s %s %s" (print_formula l) (symbol) (print_formula r)
        | Equals(l, r) -> sprintf "%s = %s" (print_formula l) (print_formula r)
        | expr -> src expr