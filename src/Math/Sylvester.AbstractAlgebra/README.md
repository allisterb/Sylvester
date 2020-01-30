```fsharp
// Use the Sylvester abstract algebra package
#load "Paket.fsx"
Paket.Package["Sylvester.AbstractAlgebra"] 
#load "Paket.Generated.Refs.fsx"

open System 
open Sylvester
```


```fsharp
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

// All types and operators are strongly types
//a ++ 4
```




    "NancyDrew"




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




```fsharp

```
