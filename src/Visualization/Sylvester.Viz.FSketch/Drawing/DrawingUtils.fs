namespace FSketch

module DrawingUtils =

    let boundariesReducer b1 b2 =
        match b1, b2 with
        | Some (left1, top1, right1, bottom1), Some (left2, top2, right2, bottom2) ->
            Some (min left1 left2, min top1 top2, max right1 right2, max bottom1 bottom2)
        | Some b1, None -> Some b1
        | None, Some b2 -> Some b2
        | _ -> None

    let minMaxReducer (min1, max1) (min2, max2) = min min1 min2, max max1 max2

    let computeBoundingPolygon shape =
        shape
        |> Pathetizer.ConvertToPath
        |> Pathetizer.GetPathPoints

    let computeShapeBoundingBox (refSpace, shape) =
        let boundingPolygon =
            computeBoundingPolygon shape.Shape
            |> List.map (fun p -> p * refSpace.transform)

        (
            boundingPolygon |> Seq.map fst |> Seq.min,
            boundingPolygon |> Seq.map snd |> Seq.min,
            boundingPolygon |> Seq.map fst |> Seq.max,
            boundingPolygon |> Seq.map snd |> Seq.max
        )

    let computeBoundingBox centerOnOrigin (shapes:Shapes) =
        match shapes with
        | [] -> None
        | _ ->
            shapes
            |> Seq.map (computeShapeBoundingBox >> Some)
            |> Seq.reduce boundariesReducer
            |> Option.map
                (fun (left, top, right, bottom) ->
                    if centerOnOrigin then
                        let x = max (abs left) (abs right)
                        let y = max (abs top) (abs bottom)
                        -x, -y, x, y
                    else
                        left, top, right, bottom)

    let recenter (shapes:Shapes) =
        match computeBoundingBox false shapes with
        | Some (left, top, right, bottom) ->
            let x = (left + right) / 2.
            let y = (top + bottom) / 2.
            shapes |> List.map (Dsl.translatedBy (-x, -y))
        | None -> shapes