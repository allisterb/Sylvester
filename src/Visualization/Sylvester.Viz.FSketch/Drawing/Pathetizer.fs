namespace FSketch

module Pathetizer =

    let private closeSubPath (pathParts: PathPart list) =
        let pathEnd = pathParts |> List.map (fun p -> p.End) |> List.sum
        match pathEnd with
        | Vector(x, y) when abs x > 0.1 || abs y > 0.1 ->
            pathParts @ [Line(Vector.Zero - pathEnd)]
        | _ ->
            pathParts

    let private convertToSubPath (pathPoints:(System.Drawing.PointF * byte) array, isClosedPath) =
        let mutable started = false
        let mutable startPosition = 0., 0.
        let pathParts =
            [
                let mutable lastPoint = new System.Drawing.PointF()
                let mutable currentIndex = 0
                while currentIndex < pathPoints.Length do
                    let point, pathType = pathPoints.[currentIndex]
                    match pathType &&& 0x7uy with
                    | 0uy ->
                        started <- true
                        startPosition <- float point.X, float point.Y
                        lastPoint <- point
                    | 1uy ->
                        yield Line(Vector(float(point.X - lastPoint.X), float(point.Y - lastPoint.Y)))
                        lastPoint <- point
                    | 3uy ->
                        let cp1 = point
                        let cp2 = fst(pathPoints.[currentIndex + 1])
                        let endPoint = fst(pathPoints.[currentIndex + 2])
                        currentIndex <- currentIndex + 2
                        yield Bezier(Vector(float(endPoint.X - lastPoint.X), float(endPoint.Y - lastPoint.Y)),
                                     Vector(float(cp1.X - lastPoint.X), float(cp1.Y - lastPoint.Y)),
                                     Vector(float(cp2.X - lastPoint.X), float(cp2.Y - lastPoint.Y)))
                        lastPoint <- endPoint
                    | _ -> ()
                    currentIndex <- currentIndex + 1
            ]

        let pathParts' = if isClosedPath then closeSubPath pathParts else pathParts

        { Start = Vector(startPosition); Parts = pathParts'; Closed = isClosedPath }

    let getBezierPointAt t ((xD, yD) as D, B, C) =
        let inline interpolateLine (x1, y1) (x2, y2) =
            x1 + (x2 - x1) * t, y1 + (y2 - y1) * t
        let ((xE, yE) as E) = interpolateLine (0., 0.) B
        let ((xF, yF) as F) = interpolateLine B C
        let ((xG, yG) as G) = interpolateLine C D
        let ((xH, yH) as H) = interpolateLine E F
        let ((xJ, yJ) as J) = interpolateLine F G
        interpolateLine H J

    let GetPathPoints (path:Path) =
        let offset = ref Vector.Zero
        let toPoint (Vector(x, y)) = x, y
        [
            for subPath in path.SubPaths do
                offset := subPath.Start
                yield !offset |> toPoint

                for pathPart in subPath.Parts do
                match pathPart with
                | Line v ->
                    offset := !offset + v
                    yield !offset |> toPoint
                | Bezier ((Vector(vx, vy) as v), (Vector(cx1, cy1) as cp1), (Vector(cx2, cy2) as cp2)) ->
                    for t in 0.0 .. 0.05 .. 1.0 do
                        let p = getBezierPointAt t ((vx, vy), (cx1, cy1), (cx2, cy2))
                        yield !offset + (Vector p) |> toPoint
                    offset := !offset + v
                    yield !offset |> toPoint
        ]

    let ConvertToPath shape =
        match shape with
        | Rectangle(Vector(width,height)) ->
            let pathParts =
                [
                    Line(Vector(width,0.))
                    Line(Vector(0.,height))
                    Line(Vector(-width,0.))
                    Line(Vector(0.,-height))
                ]
            { SubPaths = [ { Start = Vector(-width/2.,-height/2.); Parts = pathParts; Closed = true } ] }
        | Ellipse(Vector(width,height)) ->
            let x = width / 2.0
            let y = height / 2.0
            let kappa = 0.5522848
            let ox = x * kappa  // control point offset horizontal
            let oy = y * kappa // control point offset vertical
            let pathParts =
                [
                    Bezier (Vector(x, -y), Vector(0., -oy), Vector(x-ox, -y))
                    Bezier (Vector(x, y), Vector(ox, 0.), Vector(x, y-oy))
                    Bezier (Vector(-x, y), Vector(0., oy), Vector(-x+ox, y))
                    Bezier (Vector(-x, -y), Vector(-ox, 0.), Vector(-x, oy-y))
                ]
            { SubPaths = [ { Start = Vector(-x, 0.); Parts = pathParts; Closed = true } ] }
        | Path path ->
            let subPaths' =
                path.SubPaths
                |> List.map (fun s -> if s.Closed then { s with Parts = closeSubPath s.Parts } else s )
            { path with SubPaths = subPaths' }
        | Text text ->
            let i = new System.Drawing.Bitmap(1, 1)
            let g = System.Drawing.Graphics.FromImage(i)
            g.TextRenderingHint <- System.Drawing.Text.TextRenderingHint.AntiAlias
            use font = new System.Drawing.Font(text.Font.FontName, single text.Size)
            let gp = new System.Drawing.Drawing2D.GraphicsPath()
            let fontFamily = new System.Drawing.FontFamily(text.Font.FontName)
            let fontStyle = int System.Drawing.FontStyle.Regular
            let origin = new System.Drawing.PointF(0.f, 0.f)
            let stringFormat = System.Drawing.StringFormat.GenericTypographic
            stringFormat.Alignment <-
                match text.HorizontalAlign with
                | Left -> System.Drawing.StringAlignment.Near
                | Center -> System.Drawing.StringAlignment.Center
                | Right -> System.Drawing.StringAlignment.Far
            stringFormat.LineAlignment <-
                match text.VerticalAlign with
                | Top -> System.Drawing.StringAlignment.Near
                | Middle -> System.Drawing.StringAlignment.Center
                | Bottom -> System.Drawing.StringAlignment.Far
            gp.AddString(text.Text, fontFamily, fontStyle, single text.Size, origin, stringFormat)

            let isUnclosedSinglePathFont = text.Font.IsUnclosedSinglePath
            let subPathsPoints =
                 [
                    use iterator = new System.Drawing.Drawing2D.GraphicsPathIterator(gp)
                    let subPathCount = iterator.SubpathCount
                    for subPathNumber in 1 .. subPathCount do
                        let _, startIndex, endIndex, isClosed = iterator.NextSubpath()
                        let points = gp.PathPoints.[startIndex..endIndex]
                        let pathTypes = gp.PathTypes.[startIndex..endIndex]
                        yield Array.zip points pathTypes, (isClosed && not isUnclosedSinglePathFont)
                ]

            let path = { SubPaths = subPathsPoints |> List.map convertToSubPath }

            let boundingPolygon = GetPathPoints path

            let xMin = boundingPolygon |> Seq.map fst |> Seq.min
            let yMin = boundingPolygon |> Seq.map snd |> Seq.min
            let xMax = boundingPolygon |> Seq.map fst |> Seq.max
            let yMax = boundingPolygon |> Seq.map snd |> Seq.max

            let xTranslate =
                match text.HorizontalAlign with
                | Left -> -xMin
                | Center -> -(xMin + xMax) / 2.0
                | Right -> -xMax

            let yTranslate =
                match text.VerticalAlign with
                | Top -> -yMin
                | Middle -> -(yMin + yMax) / 2.0
                | Bottom -> -yMax

            let translation = Vector(xTranslate, yTranslate)

            { SubPaths = path.SubPaths |> List.map (fun sp -> { sp with Start = sp.Start + translation }) }