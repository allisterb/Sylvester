﻿namespace Sylvester

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
                script ["require(['https://cdn.jsdelivr.net/npm/jsxgraph/distrib/jsxgraphcore.js'], function(JXG) {\n" + "JXG.Options.text.useMathJax = true;" + "\n" + (src |> compile |> repbid) + "});"|> Text]
            ] |> Html.toString
            
[<RequireQualifiedAccess>]
module BoardOptions =
    let mutable Height = 640

[<AutoOpen>]
module Board =
    
    let draw_board (src:Expr) =
        let id = Guid.NewGuid().ToString()
        let repbid (s:string) = s.Replace("_bid", id)
        div [
            div [attr "id" id; attr "class" "jxgbox"; attr "style" ("width:100%;height:" + BoardOptions.Height.ToString() + "px")]
            script ["require(['https://cdn.jsdelivr.net/npm/jsxgraph/distrib/jsxgraphcore.js'], function(JXG) {\n"  + "JXG.Options.text.useMathJax = true;" + "\n" + (src |> compile |> repbid) + "});"|> Text]
        ]
        
    [<Emit("$('div.cell.selected').children('div.output_wrapper').height({0})")>]
    let setCellHeight (h:int) = ()

    [<Emit("{1}.renderer.container.style.backgroundColor = {0}")>]
    let setBoardBgColor (s:string) (b:Board)= ()

    [<Emit("{1}.renderer.container.style.height = {0};$('div.cell.selected').children('div.output_wrapper').height({0})")>]
    let setBoardHeight (h:int) (b:Board)= ()

    let to_json(x:'a) = Newtonsoft.Json.JsonConvert.SerializeObject x |> Expr.Value |> expand_as<string>
    
    [<Emit("JSON.parse({0});")>]
    let parseJson (x:string) = stub<obj>

    [<Emit("return {1}[{0}] !== undefined;")>]
    let hasProperty (prop : string) (x : obj)  = stub<bool>
    
    [<Emit("return {1}[{0}];")>]
    let getProperty<'a> (prop : string) (x : obj) = stub<'a>
    
    [<Emit("return {2}[{0}] !== undefined ? {2}[{0}] : {1};")>]
    let getPropertyOrElse<'a> (prop : string) (e:'a) (x : obj) = stub<'a>

    [<Emit("{2}[{0}] = {1};")>]
    let setProperty (prop : string) (value : obj) (x : obj) = stub<unit>

    [<JS>]
    let defaults = {||}

    [<JS>]
    let nolabel = {|name=""; withLabel = false|}

    [<JS>]
    let noface = {|size=0|}

    [<JS>]
    let invisible = {| size=0.; withLabel= false; label=""; strokeWidth = 0. |}

    [<JS>]
    let tuple_to_array (t:'a*'a) = let x, y = t in [|x; y|]

    [<JS>]
    let transparent = {|fillOpacity = 0.0|}

    [<JS>]
    let inverse = {|inverse = true|}

    [<Emit("{1}.setAttribute({0})")>]
    let withAttrs<'a when 'a :> GeometryElement> (attrs:obj) (ge:'a) = stub<'a>

    [<JS>]
    let setAttrs<'a when 'a :> GeometryElement> (attrs:obj) (ge:'a)  = withAttrs<'a> attrs ge |>  ignore

    [<JS>]
    let toString (o:obj) = o.ToString()

    [<Emit("Math.random()")>]
    let random() = stub<float>

    [<Emit("JXG.JSXGraph.initBoard(\"_bid\", {0})")>]
    let board (attr:obj) = stub<Board>

    [<JS>]
    let bbox (x1:float) (y1:float) (x2:float) (y2:float) = [|x1; y1; x2; y2|]
    
    [<JS>]
    let area d h o = [|o - d - 0.5; o + h + 0.5; o + d + 0.5; o - h - 0.5|]

    [<JS>]
    let withName (n:string) (ge:#GeometryElement) = withAttrs {| name = n |} ge 

    [<JS>]
    let withStrokeColor (n:string) (ge:#GeometryElement) = withAttrs {|strokeColor = n|} ge

    [<JS>]
    let withFillColor (n:string) (ge:#GeometryElement) = withAttrs {|fillColor = n|} ge

    [<JS>]
    let withStrokeWidth (w:float) (ge:#GeometryElement) = withAttrs {|strokeWidth = w|} ge 

    [<JS>]
    let withSmallDash (l:#Line) = withAttrs {|dash=2|} l 

    [<JS>]
    let withVal (s:Slider) (v:float) = s.setValue(v)

    [<JS>]
    let autoPosition = {|useMathJax = true; display = "html"; autoPosition = true|}

    [<JS>]
    let toFixed n f  = JXG.toFixed(f, n)
   
    [<JS>]
    let hsv2rgb h s v = JXG.hsv2rgb(h, s, v)

    [<JS>]
    let deepCopy h o = JXG.deepCopy(h, o)

    [<JS>]
    let length (l:Line) = l.L()

    [<JS>]
    let border n (p:Polygon) = p.borders.[n]

    [<JS>]
    let xaxis (b:Board) = b.defaultAxes.x

    [<JS>]
    let yaxis (b:Board) = b.defaultAxes.y

    [<Emit("{3}.create('axis', [{0}, {1}], {2})")>]
    let axis (x:real[]) (y:real[]) (attr:obj) (board:Board) = stub<Axis>
 
    [<Emit("{3}.create('axis', [{0}, {1}], {2})")>]
    let ticks (x:real[]) (y:real[]) (attr:obj) (board:Board) = stub<Ticks> 
        
    [<JS>]
    let draw_ge (board:Board) (elems: ((Board->#GeometryElement) array)) = 
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

    let darkblue = "darkblue"

    let lightbue = "lightblue"

    let pink = "pink"

    let orange = "orange"

    let gray = "gray"

    let white = "white"

[<JS; RequireQualifiedAccess>]
module rsum = 
    let left = "left"

    let right = "right"

    let upper = "upper"

    let lower = "lower"

[<AutoOpen>]
module GE =
    [<Emit("{3}.create('point', [{0}, {1}], {2})")>]
    let point (x:float) (y:float) (attr:obj) (board:Board) = stub<Point> 

    [<Emit("{3}.create('point', [{0}, {1}], {2})")>]
    let pointf (x:obj) (y:obj) (attr:obj) (board:Board) = stub<Point> 

    [<Emit("{3}.create('line', [{0}, {1}], {2})")>]
    let line (p1:obj) (p2:obj) (attr:obj) (board:Board) = stub<Line>
        
    [<Emit("{3}.create('arrow', [{0}, {1}], {2})")>]
    let arrow (p1:obj) (p2:obj) (attr:obj) (board:Board) = stub<Arrow>

    [<Emit("{3}.create('segment', [{0}, {1}], {2})")>]
    let segment (p1:obj) (p2:obj) (attr:obj) (board:Board) = stub<Segment>

    [<Emit("{2}.create('polygon', {0}, {1})")>]
    let polygon (points:obj[]) (attr:obj) (board:Board) = stub<Polygon>

    [<Emit("{3}.create('circle', [{0}, {1}], {2})")>]
    let circle (center:Point) (radius:obj) (attr:obj) (board:Board) = stub<Circle>  

    [<Emit("{4}.create('circumcircle', [{0}, {1}, {2}], {3})")>]
    let circumcircle (p1:Point) (p2:Point) (p3:Point) (attr:obj) (board:Board) = stub<Circumcircle>  

    [<Emit("{4}.create('angle', [{0}, {1}, {2}], {3})")>]
    let angle (p1:Point) (p2:Point) (p3:Point) (attr:obj) (board:Board) = stub<Angle>

    [<Emit("{5}.create('angle', [{0}, {1}, {2}, {3}], {4})")>]
    let angle_lines (l1:Line) (l2:Line) (d1:int) (d2:int) (attr:obj) (board:Board) = stub<Angle>

    [<Emit("{3}.create('midpoint', [{0}, {1}], {2})")>]
    let midpoint (p1:Point) (p2:Point) (attr:obj) (board:Board) = stub<Midpoint>

    [<Emit("{2}.create('midpoint', [{0}], {1})")>]
    let midpoint_line (l:Line) (attr:obj) (board:Board) = stub<Midpoint>
        
    [<Emit("{4}.create('intersection', [{0}, {1}, {2}], {3})")>]
    let intersection (o1:obj) (o2:obj) (i:int) (attr:obj) (board:Board) = stub<Intersection>
    
    [<Emit("{3}.create('perpendicular', [{0}, {1}], {2})")>]
    let perpendicular (l:Line) (p:Point) (attr:obj) (board:Board) = stub<Perpendicular>

    [<Emit("{3}.create('perpendicularsegment', [{0}, {1}], {2})")>]
    let perp_segment (l:Line) (p:Point) (attr:obj) (board:Board) = stub<PerpendicularSegment>

    [<Emit("{3}.create('normal', [{0}, {1}], {2})")>]
    let normal_line (l:Line) (p:Point) (attr:obj) (board:Board) = stub<Normal>

    [<Emit("{4}.create('glider', [{1}, {2}, {0}], {3})")>]
    let glider (ge:GeometryElement) (x:float) (y:float) (attr:obj) (board:Board) = stub<Glider>

    [<Emit("{7}.create('slider', [[{0}, {1}], [{0} + {2}, {1}], [{3}, {5}, {4}]], {6})")>]
    let slider (x1:float) (y:float) (x2:float) (min:float) (max:float) (start:float) (attr:obj) (board:Board) = stub<Slider>

    [<Emit("{4}.create('functiongraph', [{0}, {1}, {2}], {3})")>]
    let functiongraph (f:real->real) (min:obj) (max:obj) (attr:obj) (board:Board) = stub<Functiongraph> 

    [<Emit("{3}.create('chart', [{0}, {1}], {2})")>]
    let chart (x:real[]) (y:real[]) (attr:obj) board = stub<Chart> 

    [<Emit("{2}.create('inequality', [{0}], {1})")>]
    let inequality (l:Line) (attr:obj) board = stub<Inequality>

    [<Emit("{3}.create('curveintersection', [{0}, {1}], {2})")>]
    let curve_intersection (c1:obj) (c2:obj) (attr:obj) (board:Board) = stub<Curve>

    [<Emit("{3}.create('curvedifference', [{0}, {1}], {2})")>]
    let difference (c1:obj) (c2:obj) (attr:obj) (board:Board) = stub<Curve>

    [<Emit("{3}.create('curveunion', [{0}, {1}], {2})")>]
    let union (c1:obj) (c2:obj) (attr:obj) (board:Board) = stub<Curve>

    [<Emit("{2}.create('derivative', [{0}], {1})")>]
    let derivative (c:Curve) (attr:obj) (board:Board) = stub<Derivative> 

    [<Emit("{2}.create('integral', [{0}], {1})")>]
    let integral (c:Curve) (attr:obj) (board:Board) = stub<Integral> 

    [<Emit("{6}.create('riemannsum', [{0}, {1}, {2}, {3}, {4}], {5})")>]
    let riemannsum (f:float->float) (n:obj) (sumtype:string) (a:obj) (b:obj) (attr:obj) (board:Board) = stub<Riemannsum> 

    [<Emit("{3}.create('curve', [{0}, {1}], {2})")>]
    let curve (x:float[]) (y:float) (attr:obj) (board:Board) = stub<Curve> 

    [<Emit("{4}.create('text', [{0}, {1}, {2}], {3})")>]
    let text (x:obj) (y:obj) (s:obj) (attr:obj) (board:Board) = stub<Text>

    [<Emit("{8}.create('view3d', [[{0}, {1}], [{2}, {3}], [{4}, {5}, {6}]], {7})")>]
    let view3d (x:float) (y:float) (w:float) (h:float) (xbound:float[]) (ybound:float[]) (zbound:float[])  (attr:obj) (board:Board) = stub<View3D>

    [<Emit("{4}.create('point3d', [{0}, {1}, {2}], {3})")>]
    let point3d (x:obj) (y:obj) (z:obj) (attr:obj) (board:View3D) = stub<Point3D> 

    [<Emit("{3}.create('line3d', [{0}, {1}], {2})")>]
    let line3d (x:Point3D) (y:Point3D) (attr:obj) (view:View3D) = stub<Line3D> 

    [<Emit("{4}.create('functiongraph3d', [{0}, {1}, {2}], {3})")>]
    let functiongraph3d (f:(float*float)->float) (xrange:obj) (yrange:obj) (attr:obj) (v:View3D) = stub<Functiongraph3D>

[<RequireQualifiedAccess>]
module ge =
    [<Emit("{3}.create('point', [{0}, {1}], {2})")>]
    let point (x:float) (y:float) (attr:obj) (board:Board) = stub<GeometryElement> 

    [<Emit("{3}.create('point', [{0}, {1}], {2})")>]
    let pointf (x:obj) (y:obj) (attr:obj) (board:Board) = stub<GeometryElement> 

    [<Emit("{3}.create('line', [{0}, {1}], {2})")>]
    let line (p1:obj) (p2:obj) (attr:obj) (board:Board) = stub<GeometryElement>
        
    [<Emit("{3}.create('segment', [{0}, {1}], {2})")>]
    let segment (p1:obj) (p2:obj) (attr:obj) (board:Board) = stub<GeometryElement>

    [<Emit("{2}.create('polygon', {0}, {1})")>]
    let polygon (points:obj[]) (attr:obj) (board:Board) = stub<GeometryElement>

    [<Emit("{3}.create('circle', [{0}, {1}], {2})")>]
    let circle (center:Point) (radius:obj) (attr:obj) (board:Board) = stub<GeometryElement> 

    [<Emit("{4}.create('circumcircle', [{0}, {1}, {2}], {3})")>]
    let circumcircle (p1:Point) (p2:Point) (p3:Point) (attr:obj) (board:Board) = stub<GeometryElement>

    [<Emit("{4}.create('angle', [{0}, {1}, {2}], {3})")>]
    let angle (p1:Point) (p2:Point) (p3:Point) (attr:obj) (board:Board) = stub<GeometryElement>

    [<Emit("{5}.create('angle', [{0}, {1}, {2}, {3}], {4})")>]
    let angle_lines (l1:Line) (l2:Line) (d1:int) (d2:int) (attr:obj) (board:Board) = stub<GeometryElement>

    [<Emit("{3}.create('midpoint', [{0}, {1}], {2})")>]
    let midpoint (p1:Point) (p2:Point) (attr:obj) (board:Board) = stub<GeometryElement>

    [<Emit("{2}.create('midpoint', [{0}], {1})")>]
    let midpoint_line (l:Line) (attr:obj) (board:Board) = stub<GeometryElement>
        
    [<Emit("{4}.create('intersection', [{0}, {1}, {2}], {3})")>]
    let intersection (o1:obj) (o2:obj) (i:int) (attr:obj) (board:Board) = stub<GeometryElement>

    [<Emit("{3}.create('perpendicular', [{0}, {1}], {2})")>]
    let perpendicular (l:Line) (p:Point) (attr:obj) (board:Board) = stub<GeometryElement>

    [<Emit("{3}.create('perpendicularsegment', [{0}, {1}], {2})")>]
    let perp_segment (l:Line) (p:Point) (attr:obj) (board:Board) = stub<GeometryElement>

    [<Emit("{4}.create('glider', [{1}, {2}, {0}], {3})")>]
    let glider (ge:GeometryElement) (x:float) (y:float) (attr:obj) (board:Board) = stub<GeometryElement>

    [<Emit("{7}.create('slider', [[{0}, {1}], [{0} + {2}, {1}], [{3}, {5}, {4}]], {6})")>]
    let slider (x1:float) (y:float) (x2:float) (min:float) (max:float) (step:float) (attr:obj) (board:Board) = stub<GeometryElement>

    [<Emit("{4}.create('functiongraph', [{0}, {1}, {2}], {3})")>]
    let functiongraph (f:real->real) (min:obj) (max:obj) (attr:obj) (board:Board) = stub<GeometryElement> 

    [<Emit("{3}.create('chart', [{0}, {1}], {2})")>]
    let chart (x:real[]) (y:real[]) (attr:obj) (board:Board) = stub<GeometryElement> 

    [<Emit("{2}.create('inequality', [{0}], {1})")>]
    let inequality (l:Line) (attr:obj) (board:Board) = stub<GeometryElement>

    [<Emit("{3}.create('curveintersection', [{0}, {1}], {2})")>]
    let curve_intersection (c1:obj) (c2:obj) (attr:obj) (board:Board) = stub<GeometryElement>

    [<Emit("{3}.create('curvedifference', [{0}, {1}], {2})")>]
    let difference (c1:obj) (c2:obj) (attr:obj) (board:Board) = stub<GeometryElement>

    [<Emit("{3}.create('curveunion', [{0}, {1}], {2})")>]
    let union (c1:obj) (c2:obj) (attr:obj) (board:Board) = stub<GeometryElement>

    [<Emit("{2}.create('derivative', [{0}], {1})")>]
    let derivative (c:Curve) (attr:obj) (board:Board) = stub<GeometryElement> 

    [<Emit("{2}.create('integral', [{0}], {1})")>]
    let integral (c:Curve) (attr:obj) (board:Board) = stub<GeometryElement> 

    [<Emit("{6}.create('riemannsum', [{0}, {1}, {2}, {3}, {4}], {5})")>]
    let riemannsum (f:float->float) (n:obj) (sumtype:string) (a:obj) (b:obj) (attr:obj) (board:Board) = stub<GeometryElement> 
    
    [<Emit("{3}.create('curve', [{0}, {1}], {2})")>]
    let curve (x:float[]) (y:float) (attr:obj) (board:Board) = stub<GeometryElement> 

    [<Emit("{4}.create('text', [{0}, {1}, {2}], {3})")>]
    let text (x:obj) (y:obj) (s:obj) (attr:obj) (board:Board) = stub<GeometryElement>

    [<Emit("{8}.create('view3d', [[{0}, {1}], [{2}, {3}], [{4}, {5}, {6}]], {7})")>]
    let view3d (x:float) (y:float) (w:float) (h:float) (xbound:float[]) (ybound:float[]) (zbound:float[])  (attr:obj) (board:Board) = stub<GeometryElement>
        
    [<Emit("{4}.create('point3d', [{0}, {1}, {2}], {3})")>]
    let point3d (x:obj) (y:obj) (z:obj) (attr:obj) (board:View3D) = stub<GeometryElement>
    
    [<Emit("{3}.create('line3d', [{0}, {1}], {2})")>]
    let line3d (x:Point3D) (y:Point3D) (attr:obj) (view:View3D) = stub<GeometryElement>