#r ".\\..\\..\\src\\Providers\\Sylvester.Provider.Arithmetic\\src\\Sylvester.Provider.Arithmetic.Runtime\\bin\\Release\\netstandard2.0\\Sylvester.Provider.Arithmetic.Runtime.dll"
#r ".\\..\\..\\src\\Base\\Sylvester.Collections\\bin\\Debug\\netstandard2.0\\Sylvester.Collections.dll"
#r ".\\..\\..\\src\\Math\\Sylvester.AbstractAlgebra\\bin\\Debug\\netstandard2.0\\Sylvester.AbstractAlgebra.dll"

open Sylvester

let Z = Set(fun (_:string) -> true)

let M = Monoid(Z, (+), "")

let N = Monoid(Z, (+), "")

let S = Morph(M, N, id)