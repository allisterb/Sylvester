#load "Base10\Base10Digits.fs"
#load "Base10\Base10N5.fs"

open Sylvester.Base10
open Sylvester.Base10Digits

let n1 = N16b(D0, D0, D0, D1, D0)
let n2 = N16b(D0, D0, D0, D1, D0)
let n = n1 + n2
//let testAdd = N5d(D0, D0, D3, D1, D2) + N5d(D0, D0, D3, D2, D0)

//let testSub = N5d(D0, D0, D0, D2, D0) - N5d(D6, D0, D0, D3, D0) 

//let neg20 = !!N5d(D0, D0, D0, D2, D0)//