#r "nuget: WebSharper.Compiler"


open Microsoft.FSharp.Quotations
open WebSharper

type AR = WebSharper.Compiler.AssemblyResolver
module FE = WebSharper.Compiler.FrontEnd
 
let compile (expr: Expr) : string option =
    let loader = FE.Loader.Create (AR.Create()) (eprintfn "%O")
    FE.Loader.Create(AR (fun _ -> ())
    FE.CreateBundleJSOutput() 
    let options =
        { FE.Loader. with
            References =
                List.map loader.LoadFile [
                    // These contain the JavaScript implementation for most of the standard library
                    "WebSharper.Main.dll"
                    "WebSharper.Collections.dll"
                    "WebSharper.Control.dll"
                    // Add any other assemblies used in the quotation...
                ] }
    let compiler = FE.Content.Prepare options (eprintfn "%O")
    compiler.Compile expr
    |> Option.map (fun e -> e.ReadableJavaScript)