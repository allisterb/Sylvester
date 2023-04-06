#load "Include.fsx"

open Sylvester
open DescriptiveStatistics

let n = [|4.; 5.; 6.; 7.; 7.; 8.; 0.|]
let f = freq n
dotplot f