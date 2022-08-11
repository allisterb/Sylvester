#r "IfSharp.Kernel.dll"

#load "XPlot.Plotly.fsx"

#r"C:\\Projects\\Sylvester.git\\src\\Visualization\\Sylvester.Visualization.Web\\bin\\Debug\\netstandard2.0\\Sylvester.Visualization.Web.dll"

open FSharp.Quotations
open IfSharp.Kernel
open IfSharp.Kernel.Globals

open Sylvester

do
    /// Setup MathJax and HTML helpers
    @"<script src=""https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.7/MathJax.js?config=TeX-MML-AM_CHTML""></script>" |> Util.Html |> Display
    
    do Printers.addDisplayPrinter(fun (expr: IHtmlDisplay) -> 
        { ContentType = "text/html"; Data = expr.Html() }
        
    )

    let print_expr expr = 
        try
            let html = "$" + latex' expr + "$" in
            html
        with
        | _ -> expr.ToString()
            
    do Printers.addDisplayPrinter(fun (expr: Expr) -> 
        let html = print_expr expr in 
        { ContentType = "text/html"; Data = html }
    )
    
    do Printers.addDisplayPrinter(fun (e: Expr option) -> 
        match e with
        | Some expr -> let html = print_expr expr in { ContentType = "text/html"; Data = html }
        | None -> { ContentType = "text/html"; Data = "None" }
    )
    
    do Printers.addDisplayPrinter(fun (expr: Expr list) -> 
        let html = expr |> List.map print_expr |> List.reduce (fun a b -> a + "<br>" + b) in { ContentType = "text/html"; Data = html }
    )
    
    do Printers.addDisplayPrinter(fun (expr: Expr<int> list) -> 
        let html = expr |> List.map print_expr |> List.reduce (fun a b -> a + "<br>" + b) in { ContentType = "text/html"; Data = html }
    )
    
    do Printers.addDisplayPrinter(fun (expr: Expr<Natural> list) -> 
        let html = expr |> List.map print_expr |> List.reduce (fun a b -> a + "<br>" + b) in { ContentType = "text/html"; Data = html }
    )
    
    do Printers.addDisplayPrinter(fun (expr: Expr<rat> list) -> 
        let html = expr |> List.map print_expr |> List.reduce (fun a b -> a + "<br>" + b) in { ContentType = "text/html"; Data = html }
    )
    
    do Printers.addDisplayPrinter(fun (expr: Expr<real> list) -> 
        let html = expr |> List.map print_expr |> List.reduce (fun a b -> a + "<br>" + b) in { ContentType = "text/html"; Data = html }
    )
    
    do Printers.addDisplayPrinter(fun (e: Expr list option) -> 
        match e with
        | Some expr -> 
            let html = expr |> List.map print_expr |> List.reduce (fun a b -> a + "<br>" + b) in { ContentType = "text/html"; Data = html }
        | None -> { ContentType = "text/html"; Data = "None" }
    )
    
    do Printers.addDisplayPrinter(fun (e: Expr<int> list option) -> 
        match e with
        | Some expr -> 
            let html = expr |> List.map print_expr |> List.reduce (fun a b -> a + "<br>" + b) in { ContentType = "text/html"; Data = html }
        | None -> { ContentType = "text/html"; Data = "None" }
    )
    
    do Printers.addDisplayPrinter(fun (e: Expr<rat> list option) -> 
        match e with
        | Some expr -> 
            let html = expr |> List.map print_expr |> List.reduce (fun a b -> a + "<br>" + b) in { ContentType = "text/html"; Data = html }
        | None -> { ContentType = "text/html"; Data = "None" }
    )
    
    do Printers.addDisplayPrinter(fun (e: Expr<real> list option) -> 
        match e with
        | Some expr -> 
            let html = expr |> List.map print_expr |> List.reduce (fun a b -> a + "<br>" + b) in { ContentType = "text/html"; Data = html }
        | None -> { ContentType = "text/html"; Data = "None" }
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