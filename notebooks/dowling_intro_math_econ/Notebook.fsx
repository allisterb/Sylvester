#r "IfSharp.Kernel.dll"

open FSharp.Quotations
open IfSharp.Kernel
open IfSharp.Kernel.Globals

open Sylvester

do
    /// Setup MathJax and HTML helpers
    @"<script src=""https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.7/MathJax.js?config=TeX-MML-AM_CHTML""></script>" |> Util.Html |> Display

    Printers.addDisplayPrinter(fun (expr: Expr) ->
        try
            let html = sprint' expr in
            { ContentType = "text/html"; Data = html }
        with
        | _ -> 
            { ContentType = "text/html"; Data = (expr.ToString()) }
    )