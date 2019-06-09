#r "..\\..\\src\\Providers\\Sylvester.Provider.Arithmetic\\src\\Sylvester.Provider.Arithmetic.Runtime\\bin\\Release\\net45\\Sylvester.Provider.Arithmetic.Runtime.dll"

open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10

let g = new N<100>()

let h = one + g

check(g +< two * hundred)





