let colorType = System.Type.GetType("System.Drawing.Color")

let colors =
    typeof<System.Drawing.Color>.GetProperties(
        System.Reflection.BindingFlags.Static ||| System.Reflection.BindingFlags.Public)
    |> Seq.map(fun m ->
        let color = m.GetMethod.Invoke(null, [||]) :?> System.Drawing.Color
        let toRelative x = (double x) / 255.0
        m.Name, (color.A |> toRelative, color.R |> toRelative, color.G |> toRelative, color.B |> toRelative))
    |> Seq.toList

let genLines = seq {
    yield "#if BEHAVIOURS"
    yield "namespace FSketch.Behaviours"
    yield "#else"
    yield "namespace FSketch"
    yield "#endif"
    yield ""
    yield "open NumericOps"
    yield ""
    yield "module Colors ="
    yield ""
    for (name, (a, r, g, b)) in colors do
        yield sprintf "    let %s = ArgbColor { Alpha = ofFloat %f; R = ofFloat %f; G = ofFloat %f; B = ofFloat %f }" name a r g b
    yield ""
    yield "    let internal colorNames ="
    yield "        ["
    for (name, _) in colors do
        yield sprintf "            \"%s\", %s" (name.ToLowerInvariant()) name
    yield "        ] |> Map.ofList"

    yield ""
    yield "module NamedColor ="
    yield "     let FromName name = Colors.colorNames.[name]"

    yield ""
    yield "module Pens ="
    for (name, _) in colors do
        yield sprintf "   let %s = { Color = Colors.%s; Thickness = ofFloat 1.0; LineJoin = LineJoin.Round }" name name
    yield "   let Default = Black"
    yield ""
    yield "module Brushes ="
    for (name, _) in colors do
        yield sprintf "   let %s = SolidBrush (Colors.%s)" name name
}

System.IO.File.WriteAllLines(
    System.IO.Path.Combine(__SOURCE_DIRECTORY__, "Colors.fs"),
    genLines |> Seq.toArray)
