namespace FSketch

type Numeric = float

module NumericOps =
    let Zero = 0.
    let One = 1.
    let inline negate (x:Numeric) = -x
    let inline cos (x:Numeric) = cos x
    let inline sin (x:Numeric) = sin x
    let inline add (x:Numeric, y:Numeric) = x + y
    let inline substract (x:Numeric, y:Numeric) = x - y
    let inline multiply (x:Numeric, y:Numeric) = x * y
    let inline divide (x:Numeric, y:Numeric) = x / y
    let inline ofFloat (f:float) : Numeric = f
