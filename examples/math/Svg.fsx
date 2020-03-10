//#load "MathInclude.fsx"
#r ".\\..\\..\\src\\Visualization\\Sylvester.Viz.FSketch\\bin\\Debug\\net45\\Sylvester.Viz.FSketch.dll"

[<RequireQualifiedAccess>]
type PrinterRegistration =
    | Shapes = 0x01
    | Frame = 0x02
    | Scene = 0x04
    | RenderedScene = 0x08
    | All = 0x0F

open FSketch
open FSketch.Dsl
open FSketch.Builder
open FSketch.Svg.SvgParser

let svg = FromFile "graph.svg"
