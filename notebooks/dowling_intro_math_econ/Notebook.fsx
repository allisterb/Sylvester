#r "IfSharp.Kernel.dll"

open FSharp.Quotations
open IfSharp.Kernel
open IfSharp.Kernel.Globals

open Sylvester

do
    Printers.addDisplayPrinter(fun (expr: Expr) ->
        try
            let html = sprint' expr in
            { ContentType = "text/html"; Data = html }
        with
        | _ -> 
            { ContentType = "text/html"; Data = (expr.ToString()) }
    )