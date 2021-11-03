#load "Include.fsx"

open Sylvester
open Arithmetic


// Open Z3 module and create instance of solver
open Z3
let z3 = new Z3Solver()

// Declare 2 set variables
let A, B = setvar2<int> "A" "B"
let a = intvar "a"

let X = finite_seq [4;5;6;7]

check_sat z3 <@[ 4 |?| X @]>
check_sat z3 <@[ %a |?| Empty]@>
// Check satisfiabilty of simple set formula
check_sat z3 <@[ 3 |?| %A; 4 |?| %A; %a |?| %A;  %a = 6]@>
