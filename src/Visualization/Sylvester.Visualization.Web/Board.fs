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
    let create_board (attr:obj) = stub<Board>
        
    [<Emit("{0}.create('point', {1}, {2})")>]
    let create_point (board:Board) (coords:float[]) (attr:obj)  = stub<Point> 

    [<Emit("{0}.create('line', {1}, {2})")>]
    let create_line (board:Board) (points:Point[]) (attr:obj) = stub<Line>

    [<Emit("{0}.create('polygon', {1}, {2})")>]
    let create_polygon (board:Board) (points:Point[]) (attr:obj) = stub<Polygon>
    
    [<Emit("{0}.create('circle', {1}, {2})")>]
    let create_circle (board:Board) (children:obj[]) (attr:obj) = stub<Circle> 

    //Create: board: Board * attributes: GeometryElementAttributes * ``type``: float * oclass: float -> Circle