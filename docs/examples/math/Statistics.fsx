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

lm.Rsquared

let EARNINGS,S,EXP = realvar3  "EARNINGS" "S" "EXP"
let eawe21 = 
    csv_file "C:\\Users\Allister\\Downloads\\EAWE21.csv"
   |> with_all_col_types<float>

let m1 = eawe21 |> samples ["S"; "EARNINGS"] |> slrm (EARNINGS == b0 + b1 * S)

lrymean m1
//let mlm = eawe21 |> samples ["S"; "EXP"; "EARNINGS"] |> mlrm (EARNINGS == b0 + b1 * S + b2 * EXP)  
//mlm |> Seq.map fst |> Seq.item 0 |> Seq.toArray
//let slm = seq {for r in df -> r.["S"], r.["EARNINGS"]} |> slrm (EARNINGS == b0 + b1 * S)
//mlm


//lems witht lm = SimpleLinearRegressionModel(y .= b0 + b1 * x + u + b0, [])
