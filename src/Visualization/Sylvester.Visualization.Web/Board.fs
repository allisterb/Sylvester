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
        
    [<JS>]
    let defaults = {||}

    [<JS>]
    let tuple_to_array (t:'a*'a) = let x, y = t in [|x; y|]

    [<Emit("{0}.setAttribute({1})")>]
    let with_attrs<'a when 'a :> GeometryElement> (ge:'a) (attrs:obj) = stub<'a>

    [<JS>]
    let nolabel = {|name=""|}

    [<Emit("JXG.JSXGraph.initBoard(\"_bid\", {0})")>]
    let board (attr:obj) = stub<Board>

    [<JS>]
    let board_defaults = {| showNavigation = true; showCopyright = false; axis = true |}
    
    [<JS>]
    let bbox o d = [|o - d - 0.5; o + d + 0.5; o + d + 0.5; o - d - 0.5|]
   
    [<Emit("{3}.create('axis', [{0}, {1}], {2})")>]
    let axis (x:real[]) (y:real[]) (attr:obj) (board:Board) = stub<Axis>
 
    [<Emit("{3}.create('axis', [{0}, {1}], {2})")>]
    let ticks (x:real[]) (y:real[]) (attr:obj) (board:Board) = stub<Ticks> 
        
    

    
    
    

    [<Emit("{4}.create('line', [[{0}, {1}], [{2}, {3}]], {4})")>]
    let linexy (x1:float) (y1:float) (x2:float) (y2:float) board = stub<Line> 

  
    
    [<Emit("{1}.create('polygon', {0})")>]
    let polygonxy (points:float[][]) (board:Board) = stub<Polygon>

    [<JS>]
    let draw (board:Board) (elems: ((Board->#GeometryElement) array)) = 
        for i = 0 to elems.Length - 1 do 
            elems.[i] board |> ignore
        board

[<JS>]
module color =
    let red = "red"

    let blue = "blue"

    let green = "green"

    let yellow = "yellow"

    let black = "black"

[<JS; RequireQualifiedAccess>]
module rsum = 
    let left = "left"

    let right = "right"

[<AutoOpen>]
module GE =
    [<Emit("{3}.create('point', [{0}, {1}], {2})")>]
    let point (x:float) (y:float) (attr:obj) (board:Board) = stub<Point> 

    [<Emit("{3}.create('line', [{0}, {1}], {2})")>]
    let line (p1:obj) (p2:obj) (attr:obj) (board:Board) = stub<Line>
        
    [<Emit("{3}.create('segment', [{0}, {1}], {2})")>]
    let segment (p1:obj) (p2:obj) (attr:obj) (board:Board) = stub<Segment>

    [<Emit("{2}.create('polygon', {0}, {1})")>]
    let polygon (points:obj[]) (attr:obj) (board:Board) = stub<Polygon>

    [<Emit("{3}.create('circle', [{0}, {1}], {2})")>]
    let circle (center:Point) (radius:obj) (attr:obj) (board:Board) = stub<Circle>  

    [<Emit("{4}.create('angle', [{0}, {1}, {2}], {3})")>]
    let angle (p1:Point) (p2:Point) (p3:Point) (attr:obj) (board:Board) = stub<Angle>

    [<Emit("{5}.create('angle', [{0}, {1}, {2}, {3}], {4})")>]
    let angle_lines (l1:Line) (l2:Line) (d1:int) (d2:int) (attr:obj) (board:Board) = stub<Angle>

    [<Emit("{3}.create('midpoint', [{0}, {1}], {2})")>]
    let midpoint_pts (p1:Point) (p2:Point) (attr:obj) (board:Board) = stub<Midpoint>

    [<Emit("{2}.create('midpoint', [{0}], {1})")>]
    let midpoint_line (l:Line) (attr:obj) (board:Board) = stub<Midpoint>
        
    [<Emit("{4}.create('intersection', [{0}, {1}, {2}], {3})")>]
    let intersection (o1:obj) (o2:obj) (i:int) (attr:obj) (board:Board) = stub<Intersection>
    
    [<Emit("{3}.create('normal', [{0}, {1}], {2})")>]
    let normal (l:Line) (p:Point) (attr:obj) (board:Board) = stub<Normal>

    [<Emit("{4}.create('glider', [{1}, {2}, {0}], {3})")>]
    let glider (ge:GeometryElement) (x:float) (y:float) (attr:obj) (board:Board) = stub<Glider>

    [<Emit("{4}.create('functiongraph', [{0}, {1}, {2}], {3})")>]
    let functiongraph (f:real->real) (min:real) (max:real) (attr:obj) (board:Board) = stub<Functiongraph> 

    [<Emit("{3}.create('chart', [{0}, {1}], {2})")>]
    let chart (x:real[]) (y:real[]) (attr:obj) board = stub<Chart> 

    [<Emit("{6}.create('riemannsum', [{0}, {1}, {2}, {3}, {4}], {5})")>]
    let riemannsum (f:float->float) (n:obj) (sumtype:string) (a:float) (b:float) (attr:obj) (board:Board) = stub<Riemannsum> 

    
[<RequireQualifiedAccess>]
module ge =
    [<Emit("{3}.create('point', [{0}, {1}], {2})")>]
    let point (x:float) (y:float) (attr:obj) (board:Board) = stub<GeometryElement> 

    [<Emit("{3}.create('line', [{0}, {1}], {2})")>]
    let line (p1:obj) (p2:obj) (attr:obj) (board:Board) = stub<GeometryElement>
        
    [<Emit("{3}.create('segment', [{0}, {1}], {2})")>]
    let segment (p1:obj) (p2:obj) (attr:obj) (board:Board) = stub<GeometryElement>

    [<Emit("{2}.create('polygon', {0}, {1})")>]
    let polygon (points:obj[]) (attr:obj) (board:Board) = stub<GeometryElement>

    [<Emit("{3}.create('circle', [{0}, {1}], {2})")>]
    let circle (center:Point) (radius:obj) (attr:obj) (board:Board) = stub<GeometryElement> 

    [<Emit("{4}.create('angle', [{0}, {1}, {2}], {3})")>]
    let angle (p1:Point) (p2:Point) (p3:Point) (attr:obj) (board:Board) = stub<GeometryElement>

    [<Emit("{5}.create('angle', [{0}, {1}, {2}, {3}], {4})")>]
    let angle_lines (l1:Line) (l2:Line) (d1:int) (d2:int) (attr:obj) (board:Board) = stub<GeometryElement>

    [<Emit("{3}.create('midpoint', [{0}, {1}], {2})")>]
    let midpoint_pts (p1:Point) (p2:Point) (attr:obj) (board:Board) = stub<GeometryElement>

    [<Emit("{2}.create('midpoint', [{0}], {1})")>]
    let midpoint_line (l:Line) (attr:obj) (board:Board) = stub<GeometryElement>
        
    [<Emit("{4}.create('intersection', [{0}, {1}, {2}], {3})")>]
    let intersection (o1:obj) (o2:obj) (i:int) (attr:obj) (board:Board) = stub<GeometryElement>

    [<Emit("{4}.create('glider', [{1}, {2}, {0}], {3})")>]
    let glider (ge:GeometryElement) (x:float) (y:float) (attr:obj) (board:Board) = stub<GeometryElement>

    [<Emit("{4}.create('functiongraph', [{0}, {1}, {2}], {3})")>]
    let functiongraph (f:real->real) (min:real) (max:real) (attr:obj) (board:Board) = stub<GeometryElement> 

    [<Emit("{3}.create('chart', [{0}, {1}], {2})")>]
    let chart (x:real[]) (y:real[]) (attr:obj) board = stub<GeometryElement> 

    [<Emit("{6}.create('riemannsum', [{0}, {1}, {2}, {3}, {4}], {5})")>]
    let riemannsum (f:float->float) (n:obj) (sumtype:string) (a:float) (b:float) (attr:obj) (board:Board) = stub<GeometryElement> 
        
        
   