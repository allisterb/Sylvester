#r ".\\..\\..\\src\\Providers\\Sylvester.Provider.Arithmetic\\src\\Sylvester.Provider.Arithmetic.Runtime\\bin\\Release\\netstandard2.0\\Sylvester.Provider.Arithmetic.Runtime.dll"

open System
open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10
open Sylvester.Arithmetic.Collections

let aReg = Array.create 1000 44

let z = aReg.[2000] //Will be compiled ok but causes a runtime error

let aDep = VArray(thousand, 44)

let i = aDep.[five]

//let j = aDep.[thousand] //Type error won't compile