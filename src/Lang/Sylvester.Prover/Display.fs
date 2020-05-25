namespace Sylvester

open System.Reflection
open FSharp.Quotations
open FSharp.Quotations.DerivedPatterns
open Patterns
    
module Display = 
    let mutable transliterateGreekSymbols = true

    let (|UnicodeDisplay|_|) (f:obj) = 
        match f with
        | :? MethodInfo as info ->
            let a = info.GetCustomAttributes(typeof<UnicodeAttribute>, true) in
            if a = null || a.Length = 0 then None else let u = (a.[0] :?> UnicodeAttribute) in u.Symbol |> Some 
        | :? System.Type as t ->
            let a = t.GetCustomAttributes(typeof<UnicodeAttribute>, true) in
            if a = null || a.Length = 0 then None else let u = (a.[0] :?> UnicodeAttribute) in u.Symbol |> Some 
         | _ -> None

    let rec print_formula = 
        function
        | ForAll(_, bound, Bool true, body) -> 
            let bound' = bound |> List.fold(fun s v -> let v' = (v |> Expr.Var |> print_formula) in if s <> "" then sprintf "%s, %s" s v' else sprintf "%s" v') ""
            sprintf "\u2200 %s: (%s)" bound' (body |> print_formula)
        | ForAll(_, bound, range, body) -> 
            let bound' = bound |> List.fold(fun s v -> let v' = (v |> Expr.Var |> print_formula) in if s <> "" then sprintf "%s, %s" s v' else sprintf "%s" v') ""
            sprintf "\u2200 %s: (%s \u21D2 %s)" bound' (range |> print_formula) (body |> print_formula)
        | Exists(_, bound, Bool true, body) -> 
            let bound' = bound |> List.fold(fun s v -> let v' = (v |> Expr.Var |> print_formula) in if s <> "" then sprintf "%s, %s" s v' else sprintf "%s" v') ""
            sprintf "\u2203 %s: (%s)" bound' (body |> print_formula)
        | Exists(_, bound, range, body) -> 
            let bound' = bound |> List.fold(fun s v -> let v' = (v |> Expr.Var |> print_formula) in if s <> "" then sprintf "%s, %s" s v' else sprintf "%s" v') ""
            sprintf "\u2203 %s: (%s \u21D2 %s)" bound' (range |> print_formula) (body |> print_formula)
        | UnaryFormula(UnicodeDisplay(symbol), r) -> sprintf "%s %s" (print_formula r) (symbol)
        | BinaryFormula(UnicodeDisplay(symbol), l, r) -> sprintf "%s %s %s" (print_formula l) (symbol) (print_formula r)
        | Equals(l, r) -> sprintf "%s = %s" (print_formula l) (print_formula r)
        | expr -> src expr