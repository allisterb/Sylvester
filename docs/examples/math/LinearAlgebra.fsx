#load "Include.fsx"

open FSharp.Quotations

open Sylvester
open Sylvester.CAS
open Dimension
open Vector
open Matrix

//5 |?| Zpos
// Init Maxima CAS
//do Maxima.init "C:\\MathTools\\maxima-5.44.0\\bin\\maxima.bat"

fsi.PrintWidth <- 500
let p, q = realvar "p", realvar "q"


let A = mat [[p; q]; [2; 4]]

mmul A A

//let P = vec3 p.[0] p.[1] p.[2]

//let Q = vec3 q.[0] 4 5

//P + Q

let J = sqmat ``3``  [0; 0; p; 1; 0; -3; 0; 1; 3 + p]

let rec collect_linear_terms (expr:Expr) : Expr list=
    match expr with
    | Variable _ 
    | Constant _ -> [expr]
    | Negation _ -> [expr]
    | Multiplication(Constant c, Variable v) 
    | Multiplication(Variable v, Constant c) -> [c; v]
    | Multiplication(l, Variable v) -> collect_linear_terms l @ [v]
    | Multiplication(Variable v, r) -> [v] @ collect_linear_terms r
    | Multiplication(l, r) 
    | Addition(l, r) -> collect_linear_terms l @ collect_linear_terms r
    | Subtraction(l, Variable r) 
    | Subtraction(l, Constant r) -> collect_linear_terms l @ [call_neg r]
    | Subtraction(l, Multiplication(Constant c, Variable v)) -> collect_linear_terms l @ [call_mul (call_neg c) v]
    | Subtraction(l, r) -> collect_linear_terms l @ (List.map call_neg (collect_linear_terms r))
    | Multiplication(Variable _, Variable _) 
    | Exp(Variable _, _)
    | Exp(Variable _, _) -> failwithf "%A is not a linear expression." (sprinte expr)
    | _ -> failwithf "Could not collect linear terms from expression %A" expr
    
let eq  = p  - (7 * q***3 + q)

simplify eq
//eq.Expr.ToString()
//collect_linear_terms eq.Expr
//mprint (J + J)
//sprinte Q.[2].Expr
//sprintel [(p + q == 5.).Expr; (p - q == 1.).Expr]
//LinearEquations.solve_for p [p + q == 5.]


//let J = mat ``2`` ``2``  [p.[0]; p.[2]; 4; p.[1]]
//J * zero<dim<2>, dim<2>, _>