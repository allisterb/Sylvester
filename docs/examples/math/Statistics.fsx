#load "Include.fsx"

open Sylvester
open Sylvester.Data
open LinearRegression


let y,x = realvar2 "y" "x"
let b0,b1,b2 = realconst3 "beta_0" "beta_1" "beta_2"

let lm = slrm (y == b0 + b1 * x) [
    1,3
    2,5
    3,6
]
lm

lm

lm.Rsquared

let EARNINGS,S,FEMALE = realvar3  "EARNINGS" "S" "FEMALE"
let eawe21 = new CsvFile("C:\\Users\Allister\\Documents\\School\\ST3189\\notebooks\\EAWE21.csv")
for j in 1..eawe21.Fields.Count - 1 do eawe21.[j].Type <- typeof<float> 

let frame = new Frame(eawe21);

let mlm = query {
           for r in frame do 
           select (seq { r.["S"]; r.["FEMALE"] }, r.["EARNINGS"])
          } |> mlrm (EARNINGS == b0 + b1 * S + b2 * EARNINGS)  

let slm = seq {for r in frame -> r.["S"], r.["EARNINGS"]} |> slrm (EARNINGS == b0 + b1 * S)
//lems witht lm = SimpleLinearRegressionModel(y .= b0 + b1 * x + u + b0, [])
