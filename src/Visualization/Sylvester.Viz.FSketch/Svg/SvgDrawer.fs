namespace FSketch.Svg

open FSketch
open FSketch.Dsl
open FSketch.Builder

module internal SvgDrawerHelper =
    let toHexaColor color =
        let argbColor =
            match color with
            | ArgbColor c -> c
            | HslaColor c -> ColorSpaces.HsalToArgb c

        let r, g, b = int (argbColor.R * 255.0), int (argbColor.G * 255.0), int (argbColor.B * 255.0)
        if argbColor.Alpha > 0.999 then
            sprintf "#%02x%02x%02x" r g b
        else
            sprintf "rgba(%d, %d, %d, %f)" r g b argbColor.Alpha

    let getPathString (path:Path) =
        let getPathPartString (pathPart:PathPart) =
            match pathPart with
            | Line v ->
                sprintf "l %f,%f" v.X v.Y
            | Bezier (v, cp1, cp2) ->
                sprintf "c %f,%f %f,%f %f,%f" cp1.X cp1.Y cp2.X cp2.Y v.X v.Y
        let getSubPathStringParts (subPath:SubPath) = seq {
            yield sprintf "M %f,%f" subPath.Start.X subPath.Start.Y
            yield! subPath.Parts |> Seq.map getPathPartString
            if subPath.Closed then yield "z" }
        path.SubPaths |> Seq.collect getSubPathStringParts |> String.concat " "

    let toSvgElement (refSpace:RefSpace, styledShape) =
        let transform =
                match refSpace.transform with
                | TransformMatrix((1., 0.), (0., 1.), (0., 0.)) -> ""
                | TransformMatrix((1., 0.), (0., 1.), (mx, my)) ->
                    sprintf @" transform=""translate(%f,%f)""" mx my
                | TransformMatrix((m11, m12), (m21, m22), (mx, my)) ->
                    sprintf @" transform=""matrix(%f,%f,%f,%f,%f,%f)""" m11 m12 m21 m22 mx my

        let drawType = styledShape.DrawType
        let fill =
            match drawType with
            | Fill brush
            | ContourAndFill (_, brush) ->
                match brush with
                | SolidBrush color ->
                    sprintf "fill:%s" (toHexaColor color)
            | Contour _ -> "fill:none"
        let stroke =
            match drawType with
            | Fill _ -> "stroke:none"
            | ContourAndFill (pen, _)
            | Contour pen -> sprintf "stroke:%s; stroke-width: %f;" (toHexaColor pen.Color) pen.Thickness

        let style = sprintf @"style=""%s;%s""" fill stroke

        match styledShape.Shape with
        | Rectangle size ->
            sprintf @"<rect x=""%f"" y=""%f"" width=""%f"" height=""%f"" %s%s/>" (-size.X/2.) (-size.Y/2.)size.X size.Y style transform
        | Ellipse size ->
            sprintf @"<ellipse cx=""0"" cy=""0"" rx=""%f"" ry=""%f"" %s%s/>" (size.X/2.) (size.Y/2.) style transform
        | Path path ->
            let pathString = getPathString path
            sprintf @"<path d=""%s"" %s%s/>" pathString style transform
        | Text t as s ->
            let path = Pathetizer.ConvertToPath s
            let pathString = getPathString path
            sprintf @"<path d=""%s"" %s%s/>" pathString style transform

    let toSvgElements shapesToTranslate =
        match shapesToTranslate |> DrawingUtils.computeBoundingBox false with
        | Some (left, top, right, bottom) ->
            let translatedShapes = shapes { yield! shapesToTranslate |> at (-left, -top) }
            seq {
                yield  @"<?xml version=""1.0"" encoding=""UTF-8"" standalone=""no""?>"
                yield @"<svg"
                yield @"xmlns:dc=""http://purl.org/dc/elements/1.1/"""
                yield @"xmlns:cc=""http://creativecommons.org/ns#"""
                yield @"xmlns:rdf=""http://www.w3.org/1999/02/22-rdf-syntax-ns#"""
                yield @"xmlns:svg=""http://www.w3.org/2000/svg"""
                yield @"xmlns=""http://www.w3.org/2000/svg"""
                yield sprintf @"width=""%f""" (right-left)
                yield sprintf @"height=""%f""" (bottom-top)
                yield @"version=""1.0"">"
                yield! translatedShapes |> Seq.map toSvgElement
                yield "</svg>" }
        | None -> Seq.empty

module SvgDrawer =
    let Draw (shapes:Shapes) =
        shapes |> SvgDrawerHelper.toSvgElements |> String.concat "\n"