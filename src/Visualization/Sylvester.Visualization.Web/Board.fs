namespace Sylvester

open System

open FSharp.Quotations

open FunScript
open FunScript.Bindings
open FunScript.Bindings.JSXGraph
open Html

type BoardLayout(src:Expr) =
    member val Src = src with get, set
    member val Width = 0 with get, set
    member val Height = 0 with get, set

    interface IHtmlDisplay with
        member x.Html() = 
            let id = Guid.NewGuid().ToString()
            let repbid (s:string) = s.Replace("_bid", id)
            div [
                div [attr "id" id; attr "class" "jxgbox"; attr "style" $"width:{x.Width}px;height:{x.Height}px"]
                script ["require(['https://cdn.jsdelivr.net/npm/jsxgraph/distrib/jsxgraphcore.js'], function(JXG) {\n" + (src |> compile |> repbid) + "});"|> Text]
            ] |> Html.toString
            
[<AutoOpen>]
module Board =
    let draw_board (src:Expr) =
        let id = Guid.NewGuid().ToString()
        let repbid (s:string) = s.Replace("_bid", id)
        div [
            div [attr "id" id; attr "class" "jxgbox"; attr "style" $"width:768px;height:200px"]
            script ["require(['https://cdn.jsdelivr.net/npm/jsxgraph/distrib/jsxgraphcore.js'], function(JXG) {\n" + (src |> compile |> repbid) + "});"|> Text]
        ]
        
    [<Emit("JXG.JSXGraph.initBoard(\"_bid\", {0})")>]
    let board (attr:obj) = stub<Board>

    [<Emit("{3}.create('axis', [{0}, {1}], {2})")>]
    let axis (x:real[]) (y:real[]) (attr:obj) (board:Board) = stub<Axis>

    [<Emit("{3}.create('axis', [{0}, {1}], {2})")>]
    let ticks (x:real[]) (y:real[]) (attr:obj) (board:Board) = stub<Ticks> 
        
    [<Emit("{2}.create('point', {0}, {1})")>]
    let point (coords:real[]) (attr:obj) (board:Board) = stub<Point> 

    [<Emit("{2}.create('line', {0}, {1})")>]
    let line (points:Point[]) (attr:obj) (board:Board) = stub<Line>

    [<Emit("{2}.create('polygon', {0}, {1})")>]
    let polygon (points:Point[]) (attr:obj) (board:Board) = stub<Polygon>
    
    [<Emit("{2}.create('circle', {0}, {1})")>]
    let circle (children:obj[]) (attr:obj) (board:Board) = stub<Circle> 

    [<Emit("{4}.create('functiongraph', [{0}, {1}, {2}], {3})")>]
    let functiongraph (f:real->real) (min:real) (max:real) (attr:obj) (board:Board) = stub<Functiongraph> 

    [<Emit("{3}.create('chart', [{0}, {1}], {2})")>]
    let chart (x:real[]) (y:real[]) (attr:obj) board = stub<Chart> 