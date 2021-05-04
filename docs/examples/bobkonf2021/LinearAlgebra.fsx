#load "IncludeMath2.fsx"

open Sylvester
open Dimension
open LinearAlgbra

let m = Mat<three, two> <@[ [1.;2.];[3.;4.]; [5.;6.] ]@>