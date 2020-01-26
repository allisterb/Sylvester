#r ".\\..\\..\\src\\Providers\\Sylvester.Provider.Arithmetic\\src\\Sylvester.Provider.Arithmetic.Runtime\\bin\\Release\\netstandard2.0\\Sylvester.Provider.Arithmetic.Runtime.dll"
#r ".\\..\\..\\src\\Base\\Sylvester.Collections\\bin\\Debug\\netstandard2.0\\Sylvester.Collections.dll"
#r ".\\..\\..\\src\\Math\\Sylvester.AbstractAlgebra\\bin\\Debug\\netstandard2.0\\Sylvester.AbstractAlgebra.dll"

open System 
open System.Collections.Generic
open System.Linq
open Sylvester
open Sylvester.Arithmetic.N10

let c = infiniteSeq ((+) 65 >> Char.ConvertFromUtf32)

c.Take(26) |> Array.ofSeq

let d = Monoid(c, (+), "")

let m = Morph(d,d,id)