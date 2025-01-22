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


let drawfun() =
    let x = realvar "x"
    let f = realfun "f" (x *** 3 + 1)
    // Define a real variable and function of a single variable
    let x = realvar "x"
    let f = realfun "f" (x *** 3 + 1)
    let graph = 
        {|
            xtitle="quantity"
            ytitle="price"
            strokeColor=red
            strokeWidth=3
            title="Worldwide Coffee Supply" 
        |}
    draw graph f |> printf "%A" 

let drawfun2() = 
    let K,L = realvar2 "K" "L"
    let Kbar = realconst "K_bar"
    let QP = realfun "q" (0.1 * L * Kbar + 3 * L *** 2 * Kbar - 0.1 * L *** 3 * Kbar)
    let J = realfun "J" ((K***3) + 2. * K + 4.)
    draw {| xrange=0.,25.;yrange=(0.,500.) |} J |> printf "%A"

[<EntryPoint>]
let main argv =
        
    drawfun2() |> ignore
    0
    //let gg = WebVisualization.draw_realfun {|xrange = 0.,15.;yrange=0.,300.; a=0.,150.|} "kk" f.MapExpr |> draw_board

    //0 // return an integer exit code


