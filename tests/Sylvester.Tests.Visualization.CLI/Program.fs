// Learn more about F# at http://fsharp.org

open System
open System.Collections.Generic
open System.Linq
open FSharp.Quotations
open FunScript
open FunScript.Bindings.JSXGraph

open Sylvester
open RealNumbers
let exprvar2<'t>(n:string) = expand_as<'t>(Expr.Var(Var(n, typeof<'t>)))
[<EntryPoint>]
let main argv =
        
    let a = realconst "a"
    let r, s = realvar2 "r" "s"
    let rr = vec2 r (r + s)

    let fs = realfun "f" (min 2R 3R)
    
  
    let e = 
        <@ 
            let m = %fs.MapExpr
            m(0.)
        @>
    printf "%s" (compile e)
    0
    //let gg = WebVisualization.draw_realfun {|xrange = 0.,15.;yrange=0.,300.; a=0.,150.|} "kk" f.MapExpr |> draw_board

    //0 // return an integer exit code
