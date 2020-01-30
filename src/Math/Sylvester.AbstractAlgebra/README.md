# Sylvester.AbstractAlgebra
The Sylvester abstract algebra library contains types and operations for rigorously defining abstract algebra structures and concepts.

```fsharp
open System 
open Sylvester

// Define an infinite sequence of strings
let c = infiniteSeq ((+) 65 >> Char.ConvertFromUtf32)
c
```




    Seq (seq ["A"; "B"; "C"; "D"; ...])


```fsharp
// Define a monoid using c and the string concat operator (+)
let Cat = Monoid(c, (+), "")
Cat
```




    seq
      [("A", "B", "AB"); ("B", "C", "BC"); ("C", "D", "CD"); ("D", "E", "DE"); ...]




```fsharp
// Use a custom operator
let (++) = Cat.Op
let a, b = "Nancy", "Drew"
a++b
```




    "NancyDrew"

All structures defined by the library are strongly-typed and can use any other .NET types and operations. In the snippet below the pad function defined over sets using the ++ operation is seen not to be a homomorphism.


```fsharp
let Pad = Morph(Cat, fun x -> x.PadLeft 10)
```


```fsharp
let pad = Pad.Map
```


```fsharp
pad a ++ pad b
```




    "     Nancy      Drew"




```fsharp
pad a ++ b
```




    "     NancyDrew"

