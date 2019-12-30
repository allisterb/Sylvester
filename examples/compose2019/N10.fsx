#r ".\\..\\..\\src\\Providers\\Sylvester.Provider.Arithmetic\\src\\Sylvester.Provider.Arithmetic.Runtime\\bin\\Release\\net45\\Sylvester.Provider.Arithmetic.Runtime.dll"
//#r ".\\..\\..\\src\\Base\\Sylvester.Collections\\bin\\Debug\\net45\\Sylvester.Collections.dll"


open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10
//open Sylvester.Collections

let j = new dim<45>()

let x = j + dim<60>.create()

//let v = vanew<dim<2>, bool>

let d = one + three

let e = x + three

checklt(ten, dim<45>.create())