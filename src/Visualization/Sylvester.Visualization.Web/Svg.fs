namespace Sylvester

open SharpVG

module Svg =
    
    // Geometry
    let point x y = Point.ofInts (x, y)
    let origin = point 0 0
    let length = Length.ofInt
    let lengthf = Length.ofFloat
    let lengthem = Length.ofEm
    let lengthp = Length.ofPercent
    let area x y = Area.ofInts (x, y)
    let fromPoints = Area.fromPoints
    
    // Style
    let defaultStyle = Style.empty
    let createStyle = Style.createWithName
    let withName n style :Style = { style with  Name = Some n} 
    let withStroke color style = { style with Stroke = Some color}
    let withWidth w style = { style with StrokeWidth = Some w}
    let withFill c style = { style with Fill = Some c}
    let withFillOpacity o style = { style with FillOpacity = Some o }
    let withOpacity o style :Style = { style with  Opacity = Some o}
    
    // Shapes
    let line = Line.create
    let rect = Rect.create
    let withCornerRadius = Rect.withCornerRadius
    let circle  = Circle.create
    
    // Text
    let text = Text.create 
    let withAnchor = Text.withAnchor
    let withFont = Text.withFont
    let withFontSize = Text.withFontSize
    let withFontFamily = Text.withFontFamily

    // Viewbox
    let viewBox = ViewBox.create

    // Group
    let group = Group.ofSeq
    let groupAdd = Group.addElements
    let groupTransform = Group.addTransform

    let svg_to_html (svg:Svg) = svg.ToString()
