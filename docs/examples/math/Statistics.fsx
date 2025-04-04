#load "Include.fsx"

open MathNet.Numerics

open Sylvester
open Sylvester.Data
open RealNumbers
open LinearRegression

"C:\Users\Allister\Documents\School\EC2020\gretlfiles\wooldridge\wage1.csv" |> csv_file |> with_all_field_types<real> |> csv_fields
let y,x = realvar2 "y" "x"
let b0,b1,b2 = realconst3 "beta_0" "beta_1" "beta_2"

let salary,roe,salarydol,roedol = realvar4 "salary" "roe" "salarydol" "roedol"
let wage, educ, exper = realvar3 "wage" "educ" "exper"

let w =  csv_file "C:\Users\Allister\Documents\School\EC2020\gretlfiles\wooldridge\wage1.csv"
csv_fields w

let wage1 = 
    w
    |> samples ["educ"; "wage"] 
    |> lr (wage == b0 + b1 * educ)

lrser wage1
let wage2 = w |> samples ["educ"; "exper"; "wage"] |> lr (wage == b0 + b1*educ + b2*exper)
lrR2 wage2

lrR2 wage2

let tenure = realvar "tenure"
let b3 = realconst "beta_3"
let lw = realvar "lw"
let wage3 = w |> samples ["educ"; "exper"; "tenure"; "wage"] |> lr (wage == b0 + b1*educ + b2*exper + b3*tenure) |> change_vars [lw == ln wage]

let ceosal1 = csv_file "C:\Users\Allister\Downloads\gretlfiles\wooldridge\ceosal1.csv" |> with_all_col_types<float>
let ceo1 = ceosal1 |> samples ["roe"; "salary"] |> lr (salary == b0 + b1 * roe)
ceo1.R2


let EARNINGS,S,EXP = realvar3  "EARNINGS" "S" "EXP"
let eawe21 = 
    csv_file "C:\\Users\Allister\\Downloads\\EAWE21.csv"
    |> with_all_col_types<float>

let m1 = eawe21 |> samples ["S"; "EARNINGS"] |> lr (EARNINGS == b0 + b1 * S)

m1
let roedecl = realvar "roedecl"

let nm = ceo1 |> change_vars [
    //salarydol == 100 * salary
    roedecl == roe / 100
]

lromeqn nm

let m2 = eawe21 |> samples ["S"; "EXP"; "EARNINGS"] |> lr (EARNINGS == b0 + b1 * S + b2 * EXP)  
m2
m2 |> change_vars [roedecl == S *** 2]





lrR2 m2, lrrss m2, lrse m2, lrsd m2

let alpha =  Distributions.StudentT.InvCDF(0., 1., (real) m2.N, ((1. - 0.05) / 2.))

let alpha2 = Distributions.FisherSnedecor.InvCDF(1., 498., 0.95)
alpha2


//lems witht lm = SimpleLinearRegressionModel(y .= b0 + b1 * x + u + b0, [])
