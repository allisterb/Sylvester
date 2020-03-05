#load "MathInclude.fsx"

[<RequireQualifiedAccess>]
type PrinterRegistration =
    | Shapes = 0x01
    | Frame = 0x02
    | Scene = 0x04
    | RenderedScene = 0x08
    | All = 0x0F

#if HAS_FSI_ADDHTMLPRINTER
let RegisterPrinters (printerRegistration:PrinterRegistration) =

    if printerRegistration &&& PrinterRegistration.Shapes = PrinterRegistration.Shapes then
        fsi.AddHtmlPrinter(fun (shapes:FSketch.Shapes) ->
                                let svg = FSketch.Svg.SvgDrawer.Draw shapes
                                Seq.empty, svg)

#endif