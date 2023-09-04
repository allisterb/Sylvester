// Learn more about F# at http://fsharp.org

open System
open System.Collections.Generic
open System.Linq
open FSharp.Quotations
open FunScript
open FunScript.Bindings.JSXGraph

open Sylvester

let exprvar2<'t>(n:string) = expand_as<'t>(Expr.Var(Var(n, typeof<'t>)))
[<EntryPoint>]
let main argv =
        
    
    let r, s = realvar2 "r" "s"
    let rr = vec2 r (r + s)
    //let ggg = draw_v {|r = 6.,7. |} rr
    let gggg= WebVisualization.draw {|s = 6.,10.|} rr
    //printfn "%s" (compile (gggg.ToString()))
    //printfn "%s" (compile jj)
    0 // return an integer exit code
