#r ".\\..\\..\\src\\Providers\\Sylvester.Provider.Arithmetic\\src\\Sylvester.Provider.Arithmetic.Runtime\\bin\\Release\\netstandard2.0\\Sylvester.Provider.Arithmetic.Runtime.dll"
#r ".\\..\\..\\src\\Base\\Sylvester.Collections\\bin\\Debug\\netstandard2.0\\Sylvester.Collections.dll"

open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10
open Sylvester.Collections

let aReg = Array.create 1000 44

//let z = aReg.[2000] //Will be compiled ok but causes a runtime error

let aDep = Array<N<1000>>.create 44

//let i = aDep.[five]

let j = aDep.[thousand] //Type error won't compile