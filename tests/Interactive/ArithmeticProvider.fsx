#r "..\\..\\src\\Providers\\Sylvester.Provider.Arithmetic\\src\\Sylvester.Provider.Arithmetic.Runtime\\bin\\Release\\net45\\Sylvester.Provider.Arithmetic.Runtime.dll"

open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10


let a,b,c = new N<400>(), new N<231111>(), new N<6577700>()

let d = a + b + c

/// k(d +< ten)



