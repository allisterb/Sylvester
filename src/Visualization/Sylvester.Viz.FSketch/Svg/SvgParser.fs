namespace FSketch.Svg

open System
open System.Xml.Linq

open FSketch

type PathCommand =
    | MoveRelative
    | LineToRelative
    | CurveToRelative
    | ClosePath

type SubPathState = {
    StartX: float
    StartY: float
    LastCommand: PathCommand option
    PathParts: PathPart list }

module internal ParsingHelper =
    let emptyValues = Set.ofList ["null"; "none"; "undefined"]
    let (|EmptyValue|_|) v = if emptyValues.Contains v then Some v else None

    let stringOfChars s = s |> Seq.toArray |> String
    let skipSpacesIfAny = List.skipWhile Char.IsWhiteSpace
    let skipChar c s =
        match s with
        | c' :: s' when c = c' -> s'
        | _ -> failwithf "Cannot parse: expecting %A but was %s" c (stringOfChars s)

    let parseFloat floatString = Double.Parse(floatString, System.Globalization.CultureInfo.InvariantCulture)
    let readFloat s =
        let rec readFloat' acc s =
            match s with
            | c :: s when Char.IsDigit c -> readFloat' (c::acc) s
            | '.' :: s -> readFloat' ('.'::acc) s
            | '-' :: s -> readFloat' ('-'::acc) s
            | _ ->
                let floatString = acc |> Seq.rev |> stringOfChars
                try
                    parseFloat floatString, s
                with e -> failwithf "Cannot parse: expecting float value and was %s: %O" floatString e 
        readFloat' [] s

    let readCoordinates s =
        let x, s =  s |> skipSpacesIfAny |> readFloat
        let s = s |> skipSpacesIfAny |> skipChar ','
        let y, s = s |> skipSpacesIfAny |> readFloat
        (x, y), s

    let tryReadCommand s =
        match s |> skipSpacesIfAny with
        | 'm' :: s -> Some MoveRelative, s
        | 'l' :: s -> Some LineToRelative, s
        | 'c' :: s -> Some CurveToRelative, s
        | 'z' :: s -> Some ClosePath, s
        | _ -> None, s

    let rec parseNextCommand state s =
        match tryReadCommand s, state.LastCommand with
        | (None, []), _ -> state, false
        | (Some ClosePath, []), _ -> state, true
        | (Some MoveRelative, s), _
        | (None, s), Some MoveRelative ->
            if state.PathParts <> [] then
                failwithf "Cannot move when the path has already started: %s" (stringOfChars s)
            let (x, y), s = s |> readCoordinates
            let state = { state with LastCommand = Some LineToRelative; StartX = state.StartX + x; StartY = state.StartY + y }
            s |> parseNextCommand state
        | (Some LineToRelative, s), _
        | (None, s), Some LineToRelative ->
            let (x, y), s = s |> readCoordinates
            let part = Line(Vector(x, y))
            let state = { state with LastCommand = Some LineToRelative; PathParts = part :: state.PathParts }
            s |> parseNextCommand state
        | (Some CurveToRelative, s), _
        | (None, s), Some CurveToRelative ->
            let (cx1, cy1), s = s |> readCoordinates
            let (cx2, cy2), s = s |> readCoordinates
            let (x, y), s = s |> readCoordinates
            let part = Bezier(Vector(x, y), Vector(cx1, cy1), Vector(cx2, cy2))
            let state = { state with LastCommand = Some CurveToRelative; PathParts = part :: state.PathParts }
            s |> parseNextCommand state
        | _ -> failwithf "Cannot parse '%s'" (stringOfChars s)

    let parsePath (pathDescription:string) =
        let initialState = { LastCommand = None; StartX = 0.; StartY = 0.; PathParts = [] }
        let state, closedPath = pathDescription |> Seq.toList |> parseNextCommand initialState
        let pathParts = state.PathParts |> List.rev
        let path = { SubPaths = [ { Start = Vector.Zero; Parts = pathParts; Closed = closedPath } ] }
        RefSpace.At(state.StartX, state.StartY), path, closedPath

    let getCssProperties (s:string) =
        s.Split([|';'|], System.StringSplitOptions.RemoveEmptyEntries)
        |> Seq.map (fun s ->
            match s.Trim().Split([|':'|]) with
            | [|key; value|] -> key, value
            | _ -> failwithf "Cannot parse style %s" s)
        |> Map.ofSeq

    let private parseHex s = System.Int32.Parse(s, System.Globalization.NumberStyles.HexNumber)

    let parseColor (s:string) =
        match s.ToLowerInvariant() with
        | EmptyValue _ -> Colors.Transparent
        | hexaPattern when hexaPattern.StartsWith("#") ->
            let hexa = hexaPattern.Substring(1)
            let r, g, b =
                if hexa.Length = 3 then
                    try parseHex(hexa.Substring(0, 1)) * 17, parseHex(hexa.Substring(1, 1)) * 17, parseHex(hexa.Substring(2, 1)) * 17
                    with _ -> failwithf "Invalid hexa pattern %s" s
                elif hexa.Length = 6 then
                    try parseHex(hexa.Substring(0, 2)), parseHex(hexa.Substring(2, 2)), parseHex(hexa.Substring(4, 2))
                    with _ -> failwithf "Invalid hexa pattern %s" s
                else
                    failwithf "Invalid hexa pattern %s" s
            Color.ArgbColor { ArgbColor.Alpha = 1.; R = float r / 255.; G = float g / 255.; B = float b / 255. }
        | rgbaPattern when rgbaPattern.StartsWith("rgba(") && rgbaPattern.EndsWith(")") ->
            let middle = rgbaPattern.Substring(5, rgbaPattern.Length - 6)
            match middle.Split(',') |> Array.map (fun s -> s.Trim()) with
            | [| r; g; b; a |] ->
                try
                    let r, g, b, a = System.Double.Parse r, System.Double.Parse g, System.Double.Parse b, System.Double.Parse a
                    Color.ArgbColor { ArgbColor.Alpha = a; R = r / 255.; G = g / 255.; B = b / 255. }
                with _ -> failwithf "Invalid rgba(r, g, b, a) pattern %s" s
            | _ -> failwithf "Invalid rgba(r, g, b, a) pattern %s" s
        | name -> NamedColor.FromName s

    let parseSize (s:string) =
        if s.EndsWith("pt") then
            let size = parseFloat (s.Substring(0, s.Length - 2))
            size * 2.54 / 72.
        else
            parseFloat s

    let parseLineJoin (s:string) =
        match s with
        | "round" -> LineJoin.Round
        | EmptyValue _ -> LineJoin.Miter
        | _ -> failwithf "Cannot parse linejoin value '%s'" s

    let buildDrawType fillColor strokeColor strokeWidth strokeLineJoin =
        let fillColor =
            match fillColor with
            | Some c when c = Colors.Transparent -> None
            | Some c -> Some c
            | None -> strokeColor

        let pen = strokeColor |> Option.map (fun c -> {Color = c; Thickness = defaultArg strokeWidth 1.; LineJoin = defaultArg strokeLineJoin LineJoin.Miter })
        let brush = fillColor |> Option.map (fun c -> SolidBrush c)

        match pen, brush with
        | Some pen, Some brush ->
            ContourAndFill(pen, brush)
        | None, Some brush ->
            Fill(brush)
        | Some pen, None ->
            Contour(pen)
        | _ -> failwith "Cannot build a draw type with no pen nor brush"

    let inline xName name = XName.Get name

    let parseAttributeWith parseFunction attributeName (e:XElement) =
        match e.Attribute(xName attributeName) with
        | null -> failwithf "Missing attribute %s" attributeName
        | attribute -> 
            try
                parseFunction attribute.Value
            with e -> failwithf "Cannot parse attribute %s with value was %s: %O" attributeName attribute.Value e 

    let parseOptionalAttributeWith parseFunction attributeName (e:XElement) =
        match e.Attribute(xName attributeName) with
        | null -> None
        | attribute -> 
            try
                parseFunction attribute.Value |> Some
            with e -> failwithf "Cannot parse attribute %s with value was %s: %O" attributeName attribute.Value e 

    let parseDrawType (e:XElement) defaults =
        let properties = seq {
            for name in ["fill"; "stroke"; "stroke-width"; "stroke-linejoin"] do
                match parseOptionalAttributeWith id name e with
                | Some value -> yield name, value
                | None -> ()

            match parseOptionalAttributeWith getCssProperties "style" e with
            | Some map -> yield! map |> Map.toSeq
            | None -> ()
        }
        let mergedProperties = properties |> Seq.fold (fun m (k, v) -> Map.add k v m) (Map.ofSeq defaults)
        try
            let fillColor = mergedProperties.TryFind("fill") |> Option.map parseColor
            let strokeColor = mergedProperties.TryFind("stroke") |> Option.map parseColor
            let strokeWidth = mergedProperties.TryFind("stroke-width") |> Option.map parseSize
            let strokeLineJoin = mergedProperties.TryFind("stroke-linejoin") |> Option.map parseLineJoin
            buildDrawType fillColor strokeColor strokeWidth strokeLineJoin
        with _ -> failwithf "Cannot parse properties %A" mergedProperties

    let parseSvgElement (e:XElement) =
        match e.Name.LocalName with
        | "path" ->
            let origin, path, closed = e.Attribute(xName "d").Value |> parsePath
            let drawType = parseDrawType e []

            origin, { Shape = Path(path); DrawType = drawType }

        | "line" ->
            let x1 = parseAttributeWith parseFloat "x1" e
            let x2 = parseAttributeWith parseFloat "x2" e
            let y1 = parseAttributeWith parseFloat "y1" e
            let y2 = parseAttributeWith parseFloat "y2" e

            let drawType = parseDrawType e ["stroke","black"]

            let line = {
                Start = Vector(x1, y1)
                Parts = [Line(Vector(x2-x1, y2-y1))]
                Closed = false }

            RefSpace.Origin, { Shape = Path({SubPaths = [line]}); DrawType = drawType }

        | "ellipse" ->
            let cx = parseAttributeWith parseFloat "cx" e
            let cy = parseAttributeWith parseFloat "cy" e
            let rx = parseAttributeWith parseFloat "rx" e
            let ry = parseAttributeWith parseFloat "ry" e

            let drawType = parseDrawType e []

            RefSpace.At(cx, cy), { Shape = Ellipse(Vector(rx * 2., ry * 2. )); DrawType = drawType }

        | "rect" ->
            let x = parseAttributeWith parseFloat "x" e
            let y = parseAttributeWith parseFloat "y" e
            let width = parseAttributeWith parseFloat "width" e
            let height = parseAttributeWith parseFloat "height" e

            let drawType = parseDrawType e []

            RefSpace.At(x + width / 2., y + height / 2.), { Shape = Rectangle(Vector(width, height)); DrawType = drawType }

        | name -> failwithf "Cannot parse element %s" name

module SvgParser =
    let FromFile path =
        use stream = System.IO.File.OpenRead path
        (XDocument.Load stream).Elements().Elements()
        |> Seq.map ParsingHelper.parseSvgElement
        |> Seq.toList
