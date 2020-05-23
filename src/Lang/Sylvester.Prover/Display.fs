namespace Sylvester

open System.Reflection

open Patterns

module FormulaDisplay = 
    let get_op_unicode_display f =
        let info = f |> getFuncInfo FormulaModuleType
        match info with
        | Some i -> let a = info.Value.GetCustomAttributes(typeof<UnicodeAttribute>, true) in
                    if a = null || a.Length = 0 then None else let u = (a.[0] :?> UnicodeAttribute) in u.Symbol |> Some
        | _ -> None

    let get_op_unicode_display' (info:MethodInfo) =
        let a = info.GetCustomAttributes(typeof<UnicodeAttribute>, true) in
        if a = null || a.Length = 0 then None else let u = (a.[0] :?> UnicodeAttribute) in u.Symbol |> Some
        
    let (|UnicodeDisplay|_|) (info:MethodInfo) = get_op_unicode_display' info 

    let rec print_formula expr = 
        match expr with
        | BinaryFormula(UnicodeDisplay(symbol), l, r) -> sprintf "%s %s %s" (print_formula l) (symbol) (print_formula r) 
        | expr -> src expr