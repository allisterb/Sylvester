#load "Include.fsx"

open System.Reflection
open Sylvester
open Sylvester.CAS
open FSharp.Quotations
open Economics
open RealNumbers
open Matrix
open MathNet.Symbolics
fsi.PrintWidth <- 500


do Maxima.init "C:\\MathTools\\maxima-5.44.0\\bin\\maxima.bat"



let x = realvar "x"
let a = realconst "a"

let m = mat [[3;1;-4]; [2;5;6]; [1;4;8]]

let n = sqmat [3;2;-1;1;6;3;2;-4;0]
inverse n


let o = sqmat [1;0;0;-1;3;1;2;2;1;0;-2;1;2;0;0;1]

det o
let n = normal 4. 1.
let c = continuous_uniform 0. 1.


let X = randvar n

X.[2., 3.]

J.Distribution.CProb(0, 1)
//integrate_fun_over 2 3 c.Pdf

//em.GetVar "foo"
//let m = econ_model<ProfitMaximization>(





//let y = realvar "y"
let z = realvar "z"
let f = realfun "f" (x *** 3 + 1)
f * x
let m = econ_model<ConsumerPreference>
(m.BudgetConstraint.Fix({|Y=4.|}))
let y = realfun_im "Y" m.p1 ((m.BudgetConstraint.Fix({|Y=4.|})))


//factor (x***2 + x***4) |> latex
//partfrac_of x (4 / ((x+2)*(x*6))) |> latex



//(f.[a] + 1 + a + 6) |> fix {|a=6.|}


//solve_for_elim x [y;z] [
let eqns = [
    x - 6 *x + y == 30. - 2 * a
    x - y == 5. + a
    z == 5 * y + 2 * x
]


List.map (sexpr >> get_vars) eqns |> List.concat |> List.map (exprvar<real> >> ScalarVar<real>) |> List.distinct
       //let evars = List.except (e@[v]) allvars
       //let vars = evars@[v] |> Seq.map sexpr |> Seq.toList
type ConsumptionLeisure2() =
    inherit EconomicModel() 
    do 
        base.CreateVars("n", "C", "l", "W", "t", "h", "T", "Pi", "Ns")
        
    member x.n 
        with get() = x.GetVar("n")
        and set(value) = x.SetVar("n", value)
    member x.C 
        with get() = x.GetVar "C"
        and set(value) = x.SetVar("C", value)
    member x.l
        with get() = x.GetVar "l"
        and set(value) = x.SetVar("l", value)
    member x.W 
        with get() = x.GetVar "W"
        and set(value) = x.SetVar("W", value)
    member x.t 
        with get() = x.GetVar("t")
        and set(value) = x.SetVar("t", value)
    member x.h 
        with get() = x.GetVar("h")
        and set(value) = x.SetVar("h", value)
    member x.Ns
        with get() = x.GetVar("Ns")
        and set(value) = x.SetVar("Ns", value)
    member x.T
        with get() = x.GetVar("T")
        and set(value) = x.SetVar("T", value)
    member x.pi
        with get() = x.GetVar("Pi")
        and set(value) = x.SetVar("Pi", value)
    member x.U
        with get() = x.GetFun2<UtilityFunction2> "U" 
        and set(value:UtilityFunction2) = x.SetFun2("U", value)
    
    member val Tax:Tax option = None with get,set 
    member x.UtilityConstraints  = x.U.ScalarVars
    member x.TimeConstraint = x.l + x.Ns == x.h 
    member x.BudgetConstraint = x.C == match x.Tax with Some _ -> x.W * x.Ns + x.pi - x.T | None -> x.W * x.Ns + x.pi 

let m = econ_model<ConsumptionLeisure2>

solve_for_fun_elim m.C [m.Ns] m.W [m.BudgetConstraint; m.TimeConstraint] 

let m2 = econ_model<ConsumerPreference>
m2.U <- utilfun2 "U" ((m2.q1 *** 0.4) * (m2.q2 *** 0.6))
m2.DemandFunctions
//Maxima.last_output 10
//f.SymbolicFn.ScalarExpr.Expr

//let cp = econ_model<ConsumerPreference>()
//cp.U