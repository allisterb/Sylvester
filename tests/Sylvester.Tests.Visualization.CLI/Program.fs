// Learn more about F# at http://fsharp.org

open System
open System.Collections.Generic
open System.Linq
open FSharp.Quotations
open FunScript
open FunScript.Bindings.JSXGraph

open Sylvester
open RealNumbers
open Economics
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

let drawfun3() =
        let attrs = {|View=ConsumerPreferenceView.UtililtyMaximization; Y=(400.,300.); U = [7.; 18.; 15.; 26.]; xrange=(0.,10.);yrange=(0.,100.);strokeWidth=2|}
        let x = econ_model<ConsumerPreference> 
        x.U <- utilfun2 "U" ((x.q1 * x.q2) *** 0.5)
        let view = if has_prop<ConsumerPreferenceView> "View" attrs then get_prop<ConsumerPreferenceView> "View" attrs else failwith "A view must be specified for this consumer preference diagram"
        match view with
        | UtililtyMaximization ->
               let uvals = if has_prop<real list> "U" attrs then get_prop<real list> "U" attrs else failwith "You must specify some utility values to plot indifference curves." 
               let fs = uvals |> Seq.map(fun v -> utilfun_im "U" x.q2 (x.U == v) :> IRealFunction<RealFunction>)
               let Yrange = get_prop_or_fail<real*real> "Y" "You must specify a value for Y." attrs 
               let Y = realconst "Y"
               let y = realfun_im "Yf" x.q2 (Y == 25 * x.q1 + 5 * x.q2)
               //let p1 = get_prop_else<real*real> "p1" (0.,10.) attrs
               //let p2 = get_prop_else<real*real> "p2" (0.,10.) attrs
               let funs =fs |> Seq.map(fun x ->x.Term.MapExpr) |> Seq.append [y.MapExpr]
               let dict = to_dict attrs
               dict.["title"] <- sprintf "Consumer Preference Indifference Curves for %s" ("$$" + latex x.U + "$$")
               dict.["Y"] <- Yrange
               let names = uvals |> List.map(fun v -> (sprintf "$%s(%s, %s) = %A$" x.U.Symbol.Value x.q1.Name x.q2.Name v)) |> List.append ["Y"] |> List.toArray
               //y
               WebVisualization.draw_realfuns_dict dict names (funs |> Seq.toArray) |> draw_board
        | _ -> failwith ""     
let drawfun4() =
    // Calculate the average product of labout
    let L =realvar "L"
    let APL = realfun "AP_L" (L + 30 * L *** 2 + L *** 3)
    draw {|xrange=0.,20.;yrange=0.,300.; name1="APL"; name2="MPL";x=9|} APL
//let m = econ_model<ConsumerPreference2>
//m
//(m.BudgetConstraint.Fix({|Y=4.|}))
//let y = realfun_im "Y" m.p2 ((m.BudgetConstraint.Fix({|Y=4.|})))


//y
//m.U <- utilfun2 "U" ((m.q1 * m.q2) *** 0.5)
//drawx {|View=ConsumerPreferenceView.UtililtyMaximization; Y=(400.,300.); U = [7.; 18.; 15.; 26.]; xrange=(0.,10.);yrange=(0.,100.);strokeWidth=2|} m
[<EntryPoint>]
let main argv =
    do CAS.Maxima.init "C:\\MathTools\\maxima-5.44.0\\bin\\maxima.bat"
    drawfun4() |> printf "%A"
    0
    //let gg = WebVisualization.draw_realfun {|xrange = 0.,15.;yrange=0.,300.; a=0.,150.|} "kk" f.MapExpr |> draw_board

    //0 // return an integer exit code


