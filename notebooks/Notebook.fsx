#r "IfSharp.Kernel.dll"

open FSharp.Quotations
open IfSharp.Kernel
open IfSharp.Kernel.Globals

open Sylvester

do
    /// Setup MathJax and HTML helpers
    @"<script src=""https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.7/MathJax.js?config=TeX-MML-AM_CHTML""></script>" |> Util.Html |> Display

    let print_expr expr = 
        try
            let html = "$" + latex' expr + "$" in
            html
        with
        | _ -> expr.ToString()
            
    Printers.addDisplayPrinter(fun (expr: Expr) -> 
        let html = print_expr expr in 
        { ContentType = "text/html"; Data = html }
        
    )
    Printers.addDisplayPrinter(fun (expr: Expr*Expr) -> 
        let html = sprintf "%s, %s" (print_expr (fst expr)) (print_expr (snd expr)) in
        { ContentType = "text/html"; Data = html }
    )
    Printers.addDisplayPrinter(fun (expr: Expr<rat>*Expr<rat>) -> 
        let html = sprintf "%s, %s" (print_expr (fst expr)) (print_expr (snd expr)) in
        { ContentType = "text/html"; Data = html }
    )
    Printers.addDisplayPrinter(fun (expr: Expr<int>*Expr<int>) -> 
        let html = sprintf "%s, %s" (print_expr (fst expr)) (print_expr (snd expr)) in
        { ContentType = "text/html"; Data = html }
    )
    Printers.addDisplayPrinter(fun (expr: Expr<real>*Expr<real>) -> 
        let html = sprintf "%s, %s" (print_expr (fst expr)) (print_expr (snd expr)) in
        { ContentType = "text/html"; Data = html }
    )