#if BEHAVIOURS
namespace FSketch.Behaviours
#else
namespace FSketch
#endif

module Builder =
    type ShapesBuilder() =
        member x.Zero() = []
        member x.Yield(placedAndStyledShape:RefSpace * StyledShape) = [placedAndStyledShape]
        member x.YieldFrom(space:RefSpace, shapes:Shapes) = shapes |> List.map (fun (refSpace, shape) -> space + refSpace, shape)
        member x.Delay(f) = f()
        member x.Combine(f1, f2) = f1 @ f2
        member x.For(s, f:'T -> (RefSpace * StyledShape) list) = s |> Seq.map f |> Seq.fold (@) []

    let shapes = new ShapesBuilder()
