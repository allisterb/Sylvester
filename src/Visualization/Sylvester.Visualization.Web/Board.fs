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
            div [attr "id" id; attr "class" "jxgbox"; attr "style" "width:100%;height:512px"]
            script ["require(['https://cdn.jsdelivr.net/npm/jsxgraph/distrib/jsxgraphcore.js'], function(JXG) {\n" + (src |> compile |> repbid) + "});"|> Text]
        ]
        
    [<Emit("$('div.cell.selected').children('div.output_wrapper').height({0});\n$('div.cell.selected').children('div.jxgbox').height({0})")>]
    let setCellHeight (h:int) = ()

    [<JS>]
    let defaults = {||}

    [<JS>]
    let nolabel = {|name=""|}

    [<JS>]
    let invisible = {| size=0.; withLabel= false; label=""; strokeWidth = 0. |}

    [<JS>]
    let tuple_to_array (t:'a*'a) = let x, y = t in [|x; y|]

    [<Emit("{0}.setAttribute({1})")>]
    let withAttrs<'a when 'a :> GeometryElement> (ge:'a) (attrs:obj) = stub<'a>

    [<JS>]
    let setAttrs<'a when 'a :> GeometryElement> (ge:'a) (attrs:obj) = withAttrs<'a> ge attrs |>  ignore

    [<Emit("JXG.JSXGraph.initBoard(\"_bid\", {0})")>]
    let board (attr:obj) = stub<Board>

    [<JS>]
    let area d h o = [|o - d - 0.5; o + h + 0.5; o + d + 0.5; o - h - 0.5|]

    [<JS>]
    let with_name (n:string) (ge:#GeometryElement) = setAttrs ge {| name = n |}

    [<JS>]
    let autoPosition = {|autoPosition = true|}

    [<JS>]
    let toFixed f n = JXG.toFixed(f, n)
   
    [<Emit("{3}.create('axis', [{0}, {1}], {2})")>]
    let axis (x:real[]) (y:real[]) (attr:obj) (board:Board) = stub<Axis>
 
    [<Emit("{3}.create('axis', [{0}, {1}], {2})")>]
    let ticks (x:real[]) (y:real[]) (attr:obj) (board:Board) = stub<Ticks> 
        
    [<JS>]
    let draw (board:Board) (elems: ((Board->#GeometryElement) array)) = 
        for i = 0 to elems.Length - 1 do 
            elems.[i] board |> ignore
        board

[<JS; AutoOpen>]
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

    let upper = "upper"

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

    [<Emit("{8}.create('slider', [[{0}, {1}], [{2}, {3}], [{4}, {6}, {5}]], {7})")>]
    let slider (x1:float) (y1:float) (x2:float) (y2:float) (min:float) (max:float) (step:float) (attr:obj) (board:Board) = stub<Slider>

    [<Emit("{4}.create('functiongraph', [{0}, {1}, {2}], {3})")>]
    let functiongraph (f:real->real) (min:real) (max:real) (attr:obj) (board:Board) = stub<Functiongraph> 

    [<Emit("{3}.create('chart', [{0}, {1}], {2})")>]
    let chart (x:real[]) (y:real[]) (attr:obj) board = stub<Chart> 

    [<Emit("{2}.create('derivative', [{0}], {1})")>]
    let derivative (c:Curve) (attr:obj) (board:Board) = stub<Derivative> 

    [<Emit("{2}.create('integral', [{0}], {1})")>]
    let integral (c:Curve) (attr:obj) (board:Board) = stub<Integral> 

    [<Emit("{6}.create('riemannsum', [{0}, {1}, {2}, {3}, {4}], {5})")>]
    let riemannsum (f:float->float) (n:obj) (sumtype:string) (a:float) (b:float) (attr:obj) (board:Board) = stub<Riemannsum> 

    [<Emit("{4}.create('text', [{0}, {1}, {2}], {3})")>]
    let text (x:obj) (y:obj) (s:obj) (attr:obj) (board:Board) = stub<Text>

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

    [<Emit("{8}.create('slider', [[{0}, {1}], [{2}, {3}], [{4}, {6}, {5}]], {7})")>]
    let slider (x1:float) (y1:float) (x2:float) (y2:float) (min:float) (max:float) (step:float) (attr:obj) (board:Board) = stub<GeometryElement>

    [<Emit("{4}.create('functiongraph', [{0}, {1}, {2}], {3})")>]
    let functiongraph (f:real->real) (min:real) (max:real) (attr:obj) (board:Board) = stub<GeometryElement> 

    [<Emit("{3}.create('chart', [{0}, {1}], {2})")>]
    let chart (x:real[]) (y:real[]) (attr:obj) board = stub<GeometryElement> 

    [<Emit("{2}.create('derivative', [{0}], {1})")>]
    let derivative (c:Curve) (attr:obj) (board:Board) = stub<GeometryElement> 

    [<Emit("{2}.create('integral', [{0}], {1})")>]
    let integral (c:Curve) (attr:obj) (board:Board) = stub<GeometryElement> 

    [<Emit("{6}.create('riemannsum', [{0}, {1}, {2}, {3}, {4}], {5})")>]
    let riemannsum (f:float->float) (n:obj) (sumtype:string) (a:float) (b:float) (attr:obj) (board:Board) = stub<GeometryElement> 
    
    [<Emit("{4}.create('text', [{0}, {1}, {2}], {3})")>]
    let text (x:obj) (y:obj) (s:obj) (attr:obj) (board:Board) = stub<GeometryElement>
        
   