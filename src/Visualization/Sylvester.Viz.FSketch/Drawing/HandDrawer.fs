namespace FSketch

module HandDrawer =

    let rand = new System.Random()
    let moveRatio = 0.5

    let rec private splitLine (x, y) =
        let length = sqrt (x*x + y*y)
        if length > 10. && length < 1000. then
            let splitDistance = 4. + rand.NextDouble() * 2.
            let thX, thY = x * splitDistance / length, y * splitDistance / length
            let x' = thX + (-1. + rand.NextDouble() * 2.) * moveRatio
            let y' = thY + (-1. + rand.NextDouble() * 2.) * moveRatio
            seq {
                yield Line(Vector(x', y'))
                yield! splitLine (x - x', y - y')
            }
        else
            Seq.singleton (Line(Vector(x, y)))

    let private splitBezierAt t ((xD, yD) as D, B, C) =
        let inline interpolateLine (x1, y1) (x2, y2) =
            x1 + (x2 - x1) * t, y1 + (y2 - y1) * t
        let ((xE, yE) as E) = interpolateLine (0., 0.) B
        let ((xF, yF) as F) = interpolateLine B C
        let ((xG, yG) as G) = interpolateLine C D
        let ((xH, yH) as H) = interpolateLine E F
        let ((xJ, yJ) as J) = interpolateLine F G
        let ((xK, yK) as K) = interpolateLine H J

        let xK' = xK + (-1. + rand.NextDouble() * 2.) * moveRatio
        let yK' = yK + (-1. + rand.NextDouble() * 2.) * moveRatio

        let D' = xD - xK', yD - yK'
        let J' = xJ - xK', yJ - yK'
        let G' = xG - xK', yG - yK'

        let K' = xK', yK'

        (K', E, H), (D', J', G')

    let rec private splitBezier (x, y) (cx1, cy1) (cx2, cy2) =
        let length = sqrt (x*x + y*y)
        if length > 10. then
            let splitDistance = 4. + rand.NextDouble() * 2.
            let (K, E, H), (D, J, G) =
                ((x, y), (cx1, cy1), (cx2, cy2))
                |> splitBezierAt (splitDistance / length)

            seq {
                yield Bezier(Vector(K),Vector(E),Vector(H))
                yield! splitBezier D J G
            }
        else
            Seq.singleton (Bezier(Vector(x, y),Vector(cx1, cy1),Vector(cx2, cy2)))

    let private handDrawnPathPart pathPart =
        match pathPart with
        | Line(Vector(x, y)) ->
            splitLine (x, y)
        | Bezier(Vector(x, y), Vector(cx1, cy1), Vector(cx2, cy2)) ->
            splitBezier (x, y) (cx1, cy1) (cx2, cy2)

    let private handDrawnSubPath subPath =
        { subPath with Parts = subPath.Parts |> Seq.collect handDrawnPathPart |> Seq.toList }

    let private handDrawnPath path =
        { path with SubPaths = path.SubPaths |> List.map handDrawnSubPath }

    let RedrawByHand shapes =
        shapes
        |> List.map (fun (refSpace, { Shape = shape; DrawType = drawType }) ->
            let shape' =
                shape
                |> Pathetizer.ConvertToPath
                |> handDrawnPath
                |> Path
            (refSpace, { Shape = shape'; DrawType = drawType }))

    let RedrawAllFrames (scene:RenderedScene) =
        let frames' =
            scene.Frames
            |> Array.map (fun f -> { f with Shapes = RedrawByHand f.Shapes })

        { scene with Frames = frames' }