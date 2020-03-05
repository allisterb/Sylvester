namespace FSketch

open Dsl

type GridCellSizes = | SameHeight | SameWidth | AllSame | AllSquare | Unconstrained

type ListDrawerOptions (?defaultPen:Pen,
                        ?defaultBrush:Brush,
                        ?bordersEnabled:bool,
                        ?bordersPen:Pen,
                        ?gridCellSizes:GridCellSizes,
                        ?alwaysCenterOnOrigin:bool) =

    let defaultPen = defaultArg defaultPen Pens.Black

    member val DefaultPen = defaultPen
    member val DefaultBrush = defaultArg defaultBrush Brushes.Blue
    member val BordersEnabled = defaultArg bordersEnabled true
    member val BordersPen = defaultArg bordersPen defaultPen
    member val GridCellSizes = defaultArg gridCellSizes AllSquare
    member val AlwaysCenterOnOrigin = defaultArg alwaysCenterOnOrigin false
    
    static member Default = ListDrawerOptions()

module internal DrawingDebugUtilsInternal =

    let fromItemsShapes (options:ListDrawerOptions) itemsShapes =

        let maxY, maxX = Array2D.length1 itemsShapes - 1, Array2D.length2 itemsShapes - 1

        let shapesAndBoundaries =
            itemsShapes
            |> Array2D.map (fun shapes -> shapes, DrawingUtils.computeBoundingBox options.AlwaysCenterOnOrigin shapes)

        let getMaxBoundaries xRange yRange =
            seq {
                for y in yRange do
                for x in xRange do
                yield shapesAndBoundaries.[y, x] }
            |> Seq.map snd
            |> Seq.reduce DrawingUtils.boundariesReducer

        let rowLimits, colLimits =
            let rowLimits =
                seq {
                    for y in [0..maxY] do
                    let _, cellTop, _, cellBottom =
                        defaultArg (getMaxBoundaries [0..maxX] [y..y]) (0., 0., 0., 0.)
                    yield (cellTop + cellBottom) / 2., cellBottom - cellTop }
                |> Seq.toArray

            let colLimits =
                seq {
                    for x in [0..maxX] do
                    let cellLeft, _, cellRight, _ =
                        defaultArg (getMaxBoundaries [x..x] [0..maxY]) (0., 0., 0., 0.)
                    yield (cellLeft + cellRight) / 2., cellRight - cellLeft }
                |> Seq.toArray

            let adjustLimits maxSpace limits =
                let maxSpace =
                    match maxSpace with
                    | Some m -> m
                    | None -> limits |> Seq.map snd |> Seq.max
                limits
                |> Array.map (fun (center, space) -> center, maxSpace)

            match options.GridCellSizes with
            | Unconstrained ->
                rowLimits,
                colLimits
            | SameHeight ->
                rowLimits |> adjustLimits None,
                colLimits
            | SameWidth ->
                rowLimits,
                colLimits |> adjustLimits None
            | AllSame ->
                rowLimits |> adjustLimits None,
                colLimits |> adjustLimits None
            | AllSquare ->
                let maxSpace =  [rowLimits; colLimits] |> Seq.concat |> Seq.map snd |> Seq.max
                rowLimits |> adjustLimits (Some maxSpace),
                colLimits |> adjustLimits (Some maxSpace)

        [
            for y in 0 .. maxY do
            for x in 0 .. maxX do
                let shapes, shapesBoundaries = shapesAndBoundaries.[y, x]

                let (shiftX, shiftY) as shift =
                    (colLimits |> Seq.take x |> Seq.map snd |> Seq.sum) + (snd colLimits.[x] - snd colLimits.[0]) / 2.,
                    (rowLimits |> Seq.take y |> Seq.map snd |> Seq.sum) + (snd rowLimits.[y] - snd rowLimits.[0]) / 2.

                if options.BordersEnabled then
                    yield rectangle (snd colLimits.[x], snd rowLimits.[y]) |> at shift |> withContour options.BordersPen

                match shapesBoundaries with
                | Some (shapesLeft, shapesTop, shapesRight, shapesBottom) ->
                    let centerX, centerY = fst colLimits.[x], fst rowLimits.[y]
                    let shapesShift = shiftX - centerX, shiftY - centerY
                    for shape in shapes do
                        yield shape |> translatedBy shapesShift
                | None -> ()
        ]

    let Transform options mapper input =
        let options = defaultArg options ListDrawerOptions.Default
        let inputAsArray = List.toArray input
        Array2D.init 1 inputAsArray.Length (fun _ x -> mapper options inputAsArray.[x])
        |> fromItemsShapes options

    let Transform2D options mapper =
        let options = defaultArg options ListDrawerOptions.Default
        Array2D.map (mapper options) >> fromItemsShapes options

    let fromShapes mapper (options:ListDrawerOptions) = mapper

    let fromStyledShape mapper (options:ListDrawerOptions) =
        mapper >> at origin >> List.singleton

    let fromPath mapper (options:ListDrawerOptions) =
        mapper >> (fun p -> { Shape = Path p; DrawType = Contour options.DefaultPen }) >> at origin >> List.singleton

    let withDefaultStyle (options:ListDrawerOptions) shape =
        match shape with
        | Rectangle _
        | Ellipse _
        | Text _ -> { Shape = shape; DrawType = Fill options.DefaultBrush }
        | Path _ -> { Shape = shape; DrawType = Contour options.DefaultPen }

    let fromShape mapper (options:ListDrawerOptions) =
        mapper >> withDefaultStyle options >> at origin >> List.singleton

    let defaultFormatter o = { Shape = Text(text "%A" o); DrawType = Fill Brushes.Black }
    let defaultDebugShapes o =
        match box o with
        | :? string as s -> [RefSpace.Origin, { Shape = Text(text "%s" s); DrawType = Fill Brushes.Black }]
        | :? char as c -> [RefSpace.Origin, { Shape = Text(text "%O" c); DrawType = Fill Brushes.Black }]
        | _ -> [RefSpace.Origin, defaultFormatter o]

open DrawingDebugUtilsInternal

type DrawingDebugUtils =

    static member FromList<'a> (?options: ListDrawerOptions) : 'a list -> Shapes =
        Transform options (fromShapes defaultDebugShapes)

    static member FromList<'a> (mapper: 'a -> Shapes, ?options: ListDrawerOptions) =
        Transform options (fromShapes mapper)

    static member FromList<'a> (mapper: 'a -> StyledShape, ?options: ListDrawerOptions) =
        Transform options (fromStyledShape mapper)

    static member FromList<'a> (mapper: 'a -> Path, ?options: ListDrawerOptions) =
        Transform options (fromPath mapper)

    static member FromList<'a> (mapper: 'a -> Shape, ?options: ListDrawerOptions) =
        Transform options (fromShape mapper)

    static member FromArray2D<'a> (?options: ListDrawerOptions) : 'a[,] -> Shapes =
        Transform2D options (fromShapes defaultDebugShapes)

    static member FromArray2D<'a> (mapper: 'a -> Shapes, ?options: ListDrawerOptions) =
        Transform2D options (fromShapes mapper)

    static member FromArray2D<'a> (mapper: 'a -> StyledShape, ?options: ListDrawerOptions) =
        Transform2D options (fromStyledShape mapper)

    static member FromArray2D<'a> (mapper: 'a -> Path, ?options: ListDrawerOptions) =
        Transform2D options (fromPath mapper)

    static member FromArray2D<'a> (mapper: 'a -> Shape, ?options: ListDrawerOptions) =
        Transform2D options (fromShape mapper)

    static member AutoDraw(list) = DrawingDebugUtils.FromList() list
    static member AutoDraw(array2D) = DrawingDebugUtils.FromArray2D() array2D
    static member AutoDraw(o:obj) = defaultDebugShapes o
