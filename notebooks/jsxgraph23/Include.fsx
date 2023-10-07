#r @"C:\Users\Allister\.nuget\packages\fparsec\1.0.3\lib\netstandard1.6\FParsecCS.dll"
#r @"C:\Users\Allister\.nuget\packages\fparsec\1.0.3\lib\netstandard1.6\FParsec.dll"
#r @"C:\Users\Allister\.nuget\packages\mathnet.numerics\4.15.0\lib\netstandard2.0\MathNet.Numerics.dll"
#r @"C:\Users\Allister\.nuget\packages\mathnet.numerics.fsharp\4.15.0\lib\netstandard2.0\MathNet.Numerics.FSharp.dll"
#r @"C:\Projects\Sylvester.git\ext\mathnet-symbolics\src\Symbolics\bin\Debug\netstandard2.0\MathNet.Symbolics.dll"
#r @"C:\\Users\\Allister\\.nuget\\packages\\microsoft.z3.x64\4.8.10\\lib\\netstandard1.4\\Microsoft.Z3.dll"
#r @"C:\Projects\Sylvester.git\ext\Expect.NET\Expect.NET\bin\Debug\netstandard2.0\Expect.NETStandard.dll"
#r @"C:\\Users\\Allister\\.nuget\\packages\\xplot.plotly\\2.0.0\\lib\\netstandard2.0\\XPlot.Plotly.dll"

#r "C:\\Projects\\Sylvester.git\\src\\Math\\CAS\\Sylvester.CAS.Maxima\\bin\\Debug\\netstandard2.0\\Sylvester.Runtime.dll"
#r "C:\\Projects\\Sylvester.git\\src\\Providers\\Sylvester.Provider.Arithmetic\\src\\Sylvester.Provider.Arithmetic.Runtime\\bin\\Release\\netstandard2.0\\Sylvester.Provider.Arithmetic.Runtime.dll"
#r "C:\\Projects\\Sylvester.git\\src\\Base\\Sylvester.Collections\\bin\\Debug\\netstandard2.0\\Sylvester.Collections.dll"
#r "C:\\Projects\\Sylvester.git\\src\\Lang\\Sylvester.Expressions\\bin\\Debug\\netstandard2.0\\Sylvester.Expressions.dll"
#r "C:\\Projects\\Sylvester.git\\src\\Lang\\Sylvester.Prover\\bin\\Debug\\netstandard2.0\\Sylvester.Prover.dll"
#r "C:\\Projects\\Sylvester.git\\src\\Math\\Sylvester.AbstractAlgebra\\bin\\Debug\\netstandard2.0\\Sylvester.AbstractAlgebra.dll"
#r "C:\\Projects\\Sylvester.git\\src\\Math\\Sylvester.LinearAlgebra\\bin\\Debug\\netstandard2.0\\Sylvester.LinearAlgebra.dll"
#r "C:\\Projects\\Sylvester.git\\src\\Math\\Sylvester.RealAnalysis\\bin\\Debug\\netstandard2.0\\Sylvester.RealAnalysis.dll"
#r "C:\\Projects\\Sylvester.git\\src\\Math\\Sylvester.Statistics\\bin\\Debug\\netstandard2.0\\Sylvester.Statistics.dll"
#r "C:\\Projects\\Sylvester.git\\src\\Math\\Sylvester.Economics\\bin\\Debug\\netstandard2.0\\Sylvester.Economics.dll"

#load "XPlot.Plotly.fsx"

#r "C:\\Projects\\Sylvester.git\\src\\Math\\Sylvester.Statistics\\bin\\Debug\\netstandard2.0\\Sylvester.Statistics.dll"
#r "C:\\Projects\\Sylvester.git\\src\\Math\\CAS\\Sylvester.CAS.Maxima\\bin\\Debug\\netstandard2.0\\Sylvester.CAS.Maxima.dll"
#r "C:\\Projects\\Sylvester.git\\src\\Math\\CAS\\Sylvester.CAS.Maxima\\bin\\Debug\\netstandard2.0\\Sylvester.Runtime.dll"
#r @"C:\\Projects\\Sylvester.git\\src\\Lang\\Solvers\\Sylvester.Solver.Z3\\bin\\x64\\Debug\\netstandard2.0\\Sylvester.Solver.Z3.dll"
#r "C:\\Projects\\Sylvester.git\\src\\Visualization\\Sylvester.Visualization.Web\\bin\\Debug\\netstandard2.0\\Sylvester.Visualization.Web.dll"


#r "C:\\Projects\\Sylvester.git\\ext\\FunScript\\src\\main\\FunScript\\bin\\Debug\\netstandard2.0\\FunScript.dll"
#r "C:\\Projects\\Sylvester.git\\ext\\FunScript\\src\\main\\FunScript\\bin\\Debug\\netstandard2.0\\FunScript.Interop.NETStandard.dll"
#r "C:\\Projects\\Sylvester.git\\ext\\FunScript\\src\\main\\FunScript\\bin\\Debug\\netstandard2.0\\FunScript.Util.dll"
#r "C:\\Projects\\Sylvester.git\\ext\\FunScript\\src\\extra\\FunScript.Bindings.JSXGraph\\bin\\Debug\\netstandard2.0\\FunScript.Bindings.Base.dll"
#r "C:\\Projects\\Sylvester.git\\ext\\FunScript\\src\\extra\\FunScript.Bindings.JSXGraph\\bin\\Debug\\netstandard2.0\\FunScript.Bindings.JSXGraph.dll"

#r "IfSharp.Kernel.dll"



open FSharp.Quotations
open IfSharp.Kernel
open IfSharp.Kernel.Globals

open Sylvester
open FunScript.Bindings.JSXGraph

do
    // Setup MathJax and HTML helpers
    @"<script src=""https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.7/MathJax.js?config=TeX-MML-AM_CHTML""></script>" |> Util.Html |> Display
    
    // Setup JSXGraph
    @"<link href=""https://cdn.jsdelivr.net/npm/jsxgraph/distrib/jsxgraph.css"" rel=""stylesheet"" type=""text/css"" />" |> Util.Html |> Display
    @"<script src=""https://cdn.jsdelivr.net/npm/jsxgraph@1.4.6/distrib/jsxgraphcore.js"" type=""text/javascript"" charset=""UTF-8""></script>" |> Util.Html |> Display
    
    Printers.addDisplayPrinter(fun (expr: IHtmlDisplay) -> 
        { ContentType = "text/html"; Data = expr.Html() }
        
    )
    
    Printers.addDisplayPrinter(fun (expr: Html) -> 
        { ContentType = "text/html"; Data = Html.toString(expr) }
        
    )
    
    Printers.addDisplayPrinter(fun (expr: Expr<Board>) -> 
        { ContentType = "text/html"; Data = (draw_board expr).ToString() }
        
    )
    

    let print_expr expr = 
        try
            let html = "$" + latexe expr + "$" in
            html
        with
        | _ -> expr.ToString()
            
    
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