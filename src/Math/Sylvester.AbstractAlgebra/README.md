# Sylvester.AbstractAlgebra
The Sylvester abstract algebra library contains types and operations for rigorously defining abstract algebra structures and concepts.

```fsharp
// Use the Sylvester abstract algebra package
#load "Paket.fsx"
Paket.Package["Sylvester.AbstractAlgebra"] 
#load "Paket.Generated.Refs.fsx"

open System 
open System.Linq
open Sylvester
```

## Morphisms

```fsharp
// Define a custom symbol type S with a (+) operator and zero
// We could just also use plain strings
type S = S of string with
    static member (+) (S l, S r) = S (l + r)
    static member Zero = S ""

// Define an infinite sequence of L strings
let Sym = infiniteSeq ((+) 65 >> Char.ConvertFromUtf32 >> S)
Sym
```


```fsharp
// Define a monoid using our set and + operator and zero element
let L = Monoid(Sym, (+), S.Zero)
L
```




    seq
      [(S "A", S "B", S "AB"); (S "B", S "C", S "BC"); (S "C", S "D", S "CD");
       (S "D", S "E", S "DE"); ...]




```fsharp
// Create 2 S values
let a, b = S "Nancy", S "Drew"
a + b
```




    S "NancyDrew"




```fsharp
// Create a L morphism using the PadLeft string function
let Pad = Morph(L, fun l -> let (S s) = l in S(s.PadLeft 20))
let pad = Pad.Map
pad a
```




    S "               Nancy"




```fsharp
// Is pad a homomorphism?
pad a + pad b
```




    S "               Nancy                Drew"




```fsharp
pad (a + b)
```




    S "           NancyDrew"




```fsharp
pad a + pad b = pad (a + b)
```




    false



## Rings
```fsharp
Zpos
```




    seq [(0, 1, 1); (1, 2, 3); (2, 3, 5); (3, 4, 7); ...]


## Subsets

```fsharp
let s = seq{1..6} |> Set.ofSubsets
s
```




    Seq
      [|Empty; Seq [|1|]; Seq [|2|]; Seq [|1; 2|]; Seq [|3|]; Seq [|1; 3|];
        Seq [|2; 3|]; Seq [|1; 2; 3|]; Seq [|4|]; Seq [|1; 4|]; Seq [|2; 4|];
        Seq [|1; 2; 4|]; Seq [|3; 4|]; Seq [|1; 3; 4|]; Seq [|2; 3; 4|];
        Seq [|1; 2; 3; 4|]; Seq [|5|]; Seq [|1; 5|]; Seq [|2; 5|]; Seq [|1; 2; 5|];
        Seq [|3; 5|]; Seq [|1; 3; 5|]; Seq [|2; 3; 5|]; Seq [|1; 2; 3; 5|];
        Seq [|4; 5|]; Seq [|1; 4; 5|]; Seq [|2; 4; 5|]; Seq [|1; 2; 4; 5|];
        Seq [|3; 4; 5|]; Seq [|1; 3; 4; 5|]; Seq [|2; 3; 4; 5|];
        Seq [|1; 2; 3; 4; 5|]; Seq [|6|]; Seq [|1; 6|]; Seq [|2; 6|];
        Seq [|1; 2; 6|]; Seq [|3; 6|]; Seq [|1; 3; 6|]; Seq [|2; 3; 6|];
        Seq [|1; 2; 3; 6|]; Seq [|4; 6|]; Seq [|1; 4; 6|]; Seq [|2; 4; 6|];
        Seq [|1; 2; 4; 6|]; Seq [|3; 4; 6|]; Seq [|1; 3; 4; 6|]; Seq [|2; 3; 4; 6|];
        Seq [|1; 2; 3; 4; 6|]; Seq [|5; 6|]; Seq [|1; 5; 6|]; Seq [|2; 5; 6|];
        Seq [|1; 2; 5; 6|]; Seq [|3; 5; 6|]; Seq [|1; 3; 5; 6|]; Seq [|2; 3; 5; 6|];
        Seq [|1; 2; 3; 5; 6|]; Seq [|4; 5; 6|]; Seq [|1; 4; 5; 6|];
        Seq [|2; 4; 5; 6|]; Seq [|1; 2; 4; 5; 6|]; Seq [|3; 4; 5; 6|];
        Seq [|1; 3; 4; 5; 6|]; Seq [|2; 3; 4; 5; 6|]; Seq [|1; 2; 3; 4; 5; 6|]|]



## Lattices


```fsharp

let lat = Lattice(s, (|+|), (|*|))
lat
```




    Sylvester.Lattice`1[Sylvester.Set`1[System.Int32]]




```fsharp

```
