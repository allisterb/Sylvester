#load "Include.fsx"

open Sylvester
open Sylvester.Data
open LinearRegression


let y,x = realvar2 "y" "x"
let b0,b1,b2 = realconst3 "beta_0" "beta_1" "beta_2"

let lm = slrm' (y == b0 + b1 * x) [
    1,3
    2,5
    3,6
]
lm

lm


let EARNINGS,S,EXP = realvar3  "EARNINGS" "S" "EXP"
let eawe21 = 
    csv_file "C:\\Users\Allister\\Downloads\\EAWE21.csv"
    |> with_all_col_types<float>

let m1 = eawe21 |> samples ["S"; "EARNINGS"] |> slrm (EARNINGS == b0 + b1 * S)

let m2 = eawe21 |> samples ["S"; "EXP"; "EARNINGS"] |> mlrm (EARNINGS == b0 + b1 * S + b2 * EXP)  

lrR2 m2, lrrss m2, lrse m2, lrsd m2

//lems witht lm = SimpleLinearRegressionModel(y .= b0 + b1 * x + u + b0, [])
