#r ".\\..\\..\\src\\Providers\\Sylvester.Provider.Arithmetic\\src\\Sylvester.Provider.Arithmetic.Runtime\\bin\\Release\\net45\\Sylvester.Provider.Arithmetic.Runtime.dll"
#r ".\\..\\..\\src\\Base\\Sylvester.Collections\\bin\\Debug\\net45\\Sylvester.Collections.dll"

open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10
open Sylvester.Collections

//Type-level arithmetic
let a = new n<500>()

let b = new n<125>()

let c = a + b


//Type-level comparison
let d = c +> a

let e = c * a +< b

//Type-level static checks
check(b +> zero)
//check(b +< zero)
c