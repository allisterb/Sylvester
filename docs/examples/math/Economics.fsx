#load "Include.fsx"

open Sylvester
open Sylvester.CAS

open System
open System.Collections.Generic
open System.Linq
open System.Reflection
open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns
open Dimension
open Economics
open LinearEquations
open FunScript

do Maxima.init "C:\\MathTools\\maxima-5.44.0\\bin\\maxima.bat"

//Declare 2 real variables
let p,q = realvar2 "p" "q"
//Declare 3 real constants representing the price of sugar, price of chocolate, and consumer income respectively
let ps, pc, Y = realconst3 "p_s" "p_c" "Y"

// Declare 2more real symbolic variables
let L, K = realvar2 "L" "K"
// Declare 4 more real symbolic constamts
let alpha, beta, A, Kbar  = realconst4 "alpha" "beta" "A" "K_bar"

let QD = demandfun "Q_d" (8.56 - p - 0.3 * ps + 0.1 * Y)

let QD1 = fix {|``p++s``=0.2; Y=35.|} QD

marginal p QD1

let me = create_econ_model<MarketEquilibrium>()
me.Qd <- demandfun "Qd" (me.p + 0.5)
me.Qs <- supplyfun "Qs" (2 * me.p + 2.5)
solve_for_econ_var me.p [me.MarketEquilibrium]
//let u = utilfun "Q_u" (8.56 - p - 0.3 * ps + 0.1 * Y)
//let u2 = u :> IRealFunction<RealFunction>
//let u2 = fix {|Y=4.; p_s=3.|} u
//u2 
// let q1 be a short-run production function holding K constant
//let QP = prodfun "q" (0.1 * L * Kbar + 3 * L *** 2 * Kbar - 0.1 * L *** 3 * Kbar)

//let QP1 = fix {|K_bar=10.|} QP |> simplify
//QP1

(*
let A = realconst "A"
let a, b = realconst2 "a" "b"
let r, s = realvar2 "r" "s"
let p,q = realvar2 "p" "q"
let p1,p2 = realconst2 "p_1" "p_2"
let q1,q2 = realvar2 "q_1" "q_2"


let L, K = realvar2 "L" "K"
let Y = realconst "Y"
let lambda = realvar "lam"

let C = costfun "C" (100 * q - 4 * q *** 2 + 0.2 * q *** 3 + 450)
let C3 = costfun "C3" (q***2 + 2 * q)

let Qd1 = realfun "Q_d" (12 - p)
let Qs1 = realfun "Q_s" (9 + 0.5 * p)

//Qs1 == [Qd1] |> solve_for_econ_var p
let C2 = realfun_im_pos_vars "C2" K (6. ==  (L *** 0.5 * K *** 0.5))

let MC = marginal q C
MC

let U1 = utilfun2 "U" (q1 *** a * q2 *** (1-a))

let bc = budget_constraint (Y == p1 * q1 - p2 * q2)
let L1 = U1.[q1, q2] + lambda * (Y - p1 * q1 - p2 * q2)
let lc1 = diff q1 L1 == 0.
let lc2 = diff q2 L1 == 0.
let lc3 = diff lambda L1 == 0.
diff q1 L1 |> simplify

let s1 = solve_for_econ_var_unique lambda [lc1] 
let s2 = solve_for_econ_var_unique lambda [lc2]
solve_for_econ_var_unique q1 [lambda == s1; lambda == s2; (Y == p1 * q1 - p2 * q2)] 
//s1 == s2
*)