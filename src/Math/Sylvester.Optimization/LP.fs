namespace Sylvester

open FSharp.Quotations

open Z3

type LPSol =
| IntLPSol of (string * int) list 
| RatLPSol of (string * rat) list 

module LP =
    let max (f:Expr<'t>) (constraints:Expr<bool list>)  =
        use z3 = new Z3Solver()
        opt_assert_hard z3 constraints
        let h = opt_maximize z3 f
        match (f.Type).Name with
            | "Int32" -> Option.map IntLPSol (opt_get_int_var_model z3)
            | "Double" 
            | "Rational" -> Option.map RatLPSol (opt_get_rat_var_model z3)
            | _ -> None