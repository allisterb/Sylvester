#if BEHAVIOURS
namespace FSketch.Behaviours
#else
namespace FSketch
#endif

open Microsoft.FSharp.Math
open NumericOps

type Vector = Vector of Numeric * Numeric with
    static member Zero = Vector (Zero, Zero)
    static member (~-) (Vector(x1, y1)) = Vector(negate x1, negate y1)
    static member (+) (Vector(x1, y1) , Vector(x2, y2)) = Vector(add(x1, x2), add(y1, y2))
    static member (-) (Vector(x1, y1) , Vector(x2, y2)) = Vector(substract(x1, x2), substract(y1, y2))
    static member (*) (Vector(x1, y1) , ratio) = Vector(multiply(ratio, x1), multiply(ratio, y1))
    static member (*) (ratio, Vector(x1, y1)) = Vector(multiply(ratio, x1), multiply(ratio, y1))
    member this.X = match this with | Vector(x, _) -> x
    member this.Y = match this with | Vector(_, y) -> y
    override this.ToString() = sprintf "%A" this

type TransformMatrix =
    | TransformMatrix of (Numeric * Numeric) * (Numeric * Numeric) * (Numeric * Numeric) with
    member this.x = match this with | TransformMatrix(_,_,(x,_)) -> x
    member this.y = match this with | TransformMatrix(_,_,(_,y)) -> y
    static member (*) (x, y) =
        let inline (+) x y = add(x, y)
        let inline (*) x y = multiply(x, y)
        match x, y with
        | TransformMatrix((m11, m12), (m21, m22), (mx, my)),
          TransformMatrix((n11, n12), (n21, n22), (nx, ny)) ->
            TransformMatrix((m11 * n11 + m12 * n21, m11 * n12 + m12 * n22),
                            (m21 * n11 + m22 * n21, m21 * n12 + m22 * n22),
                            (mx * n11 + my * n21 + nx, mx * n12 + my * n22 + ny))
    static member (*) (x, ratio) =
        let inline (+) x y = add (x, y)
        let inline (-) x y = substract (x, y)
        let inline (*) x y = multiply (x, y)
        match x with
        | TransformMatrix((m11, m12), (m21, m22), (mx, my)) ->
            TransformMatrix(((m11 - One) * ratio + One, m12 * ratio),
                            (m21 * ratio, (m22 - One) * ratio + One),
                            (mx * ratio, my * ratio))
    static member (*) ((x, y), TransformMatrix((m11, m12), (m21, m22), (mx, my))) =
        let inline (+) x y = add(x, y)
        let inline (*) x y = multiply(x, y)
        x * m11 + y * m21 + mx, x * m12 + y * m22 + my 
    override this.ToString() = sprintf "%A" this

module Transforms =
    let id = TransformMatrix((One, Zero), (Zero, One), (Zero, Zero))
    let rotate alpha = TransformMatrix((cos alpha, sin alpha), (negate(sin alpha), cos alpha), (Zero, Zero))
    let translate (x, y) = TransformMatrix((One, Zero), (Zero, One), (x, y))
    let scale ratio = TransformMatrix((ratio, Zero), (Zero, ratio), (Zero, Zero))
    let scaleX ratio = TransformMatrix((ratio, Zero), (Zero, One), (Zero, Zero))
    let scaleY ratio = TransformMatrix((One, Zero), (Zero, ratio), (Zero, Zero))
    let flipX = scaleX (negate One)
    let flipY = scaleY (negate One)

type RefSpace = { transform:TransformMatrix; z:Numeric } with
    static member Origin = { transform = Transforms.translate (Zero, Zero); z = Zero }
    static member At(x, y) = { transform = Transforms.translate (x, y); z = Zero }
    static member Transform(transform) = { transform = transform; z = Zero }
    static member (+) (s1, s2) = { transform = s2.transform * s1.transform; z = add (s1.z, s2.z) }
    member this.x = this.transform.x
    member this.y = this.transform.y
    override this.ToString() = sprintf "%A" this

type ArgbColor = { Alpha: Numeric; R: Numeric; G: Numeric; B: Numeric}
type HslaColor = { H: Numeric; S: Numeric; L: Numeric; Alpha: Numeric }

type Color = | ArgbColor of ArgbColor | HslaColor of HslaColor

type [<RequireQualifiedAccess>] LineJoin = | Miter | Round

type Pen = { Color:Color; Thickness:Numeric; LineJoin:LineJoin }

type Brush =
    | SolidBrush of Color with
    static member FromColor(color) = SolidBrush color

type PathPart =
    | Line of Vector:Vector
    | Bezier of Vector:Vector * cp1:Vector * cp2:Vector
    with member x.End = match x with
                        | Line v -> v
                        | Bezier (v, _, _) -> v

type SubPath = {
    Start: Vector
    Parts: PathPart list
    Closed: bool }
    with member x.End = if x.Closed then x.Start else x.Parts |> List.map (fun p -> p.End) |> List.sum

type Path = {
    SubPaths: SubPath list }

type DrawType =
    | Contour of Pen
    | Fill of Brush
    | ContourAndFill of Pen * Brush with
    member x.Pen = match x with | Contour(p) | ContourAndFill(p, _) -> Some p | _ -> None
    member x.Brush = match x with | Fill(b) | ContourAndFill(_, b) -> Some b | _ -> None
    override this.ToString() = sprintf "%A" this

[<RequireQualifiedAccess>]
type Font =
    | Arial
    | UnclosedSinglePathFont of string with
    member this.FontName =
        match this with
        | Font.Arial -> "Arial"
        | Font.UnclosedSinglePathFont name -> name
    member this.IsUnclosedSinglePath =
        match this with
        | Font.Arial -> false
        | Font.UnclosedSinglePathFont _ -> true

type HorizontalAlign = | Left | Center | Right
type VerticalAlign = | Top | Middle | Bottom
type Text = {
    Text: string
    Size: Numeric
    Font: Font
    VerticalAlign: VerticalAlign
    HorizontalAlign: HorizontalAlign }

type Shape =
    | Rectangle of Size:Vector
    | Ellipse of Size:Vector
    | Path of Path
    | Text of Text with
    override this.ToString() = sprintf "%A" this

type StyledShape = {
    Shape: Shape
    DrawType: DrawType }

and Shapes = (RefSpace * StyledShape) list