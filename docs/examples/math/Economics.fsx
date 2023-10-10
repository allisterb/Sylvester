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
open Microeconomics
open LinearEquations
open FunScript

do Maxima.init "C:\\MathTools\\maxima-5.44.0\\bin\\maxima.bat"

let z3sol = new Z3Solver()

let x,y,z = intvar "x", intvar "Y", intvar "z"

let A = realconst "A"
let a, b = realconst2 "a" "b"
let r, s = realvar2 "r" "s"
let p,q = realvar2 "p" "q"

let L, K = realvar2 "L" "K"
let C = costfun "C" (100 * q - 4 * q *** 2 + 0.2 * q *** 3 + 450)
let C3 = costfun "C3" (q***2 + 2 * q)

let Qd1 = realfun "Q_d" (12 - p)
let Qs1 = realfun "Q_s" (9 + 0.5 * p)

Qs1 == Qd1 |> solve_for_econ_var p
let C2 = realfun_im_pos_vars "C2" K (6. ==  (L *** 0.5 * K *** 0.5))
C2
let MC = marginal q C
MC.Symbol

let U1 = utilfun2 "U" (p *** 0.5 * q *** 0.5)



U1.[4, 5]
let ic = utilfun_im "U" p (U1.[p, q] == 11.)

//let AC = realfun "AC" (C.[q] / q)

let P = prodfun2 "Q" (A * L *** a * K *** b)
fixconst {|a=7.; b=6.5|} P

let P3 = realfun "P" (L + 4 * 5)
(P3 :> IHtmlDisplay).Html()
marginal L P


trigreduce (2* sin (r) * cos r)

(Maxima.last_output 10).ToUpper()
partfrac_of (r) (3 / (4 * (1 + r)))
maximize z3sol [2*(r + s) == 100.] (r*s)
let a, b = realconst2 "a" "b"
let f = realfun (r***2 + r*a + a)

(diff r f).Body 
   
(Maxima.last_output 10

let P, Q, J = realvar "P", realvar "Q", realvar "J"

simplify ( P * (Q + Q))

Maxima.last_output(50)
let U = realvar "U"
let A = realconst "A"
//P + Q == 5. |> sexpr
let Qs = demand (P .= 2 * Q + 4 + A)

Qs.[A]

open FSharp.Quotations
open System.Collections.Generic
let exprvar2<'t>(n:string) = expand_as<'t>(Expr.Var(Var(n, typeof<'t>)))


let draw_v (attrs:'a) (v:Vec<dim<2>>) = 
    let origin = if has_prop "origin" typeof<real*real> attrs then get_prop "origin" typeof<real*real> attrs :?> real*real else 0. ,0.
    let originx, originy = exprv <| fst origin, exprv <| snd origin 
    let mutable m0 = v.Expr.[0].Raw 
    let mutable m1 = v.Expr.[1].Raw
    let _sliders0 = new Dictionary<string, obj>()
    let s0v = Expr.Var(Var("sliders0", typeof<Dictionary<string, FunScript.Bindings.JSXGraph.Slider>>)) |> expand_as<Dictionary<string, FunScript.Bindings.JSXGraph.Slider>>
    let _sliders1 = new System.Collections.Generic.Dictionary<string, obj>()
    let s1v = Expr.Var(Var("sliders1", typeof<Dictionary<string, FunScript.Bindings.JSXGraph.Slider>>)) |> expand_as<Dictionary<string, FunScript.Bindings.JSXGraph.Slider>>
    
    get_consts m0 |> List.iter(fun (t, n) -> if has_prop n typeof<real> attrs then m0 <- replace_expr (Expr.ValueWithName(0., n)) (Expr.Value(get_prop n typeof<real> attrs, typeof<real>)) m0)
    get_vars m0 |> List.iter(fun v -> if has_prop v.Name v.Type attrs then m0 <- replace_expr (Expr.Var(v)) (Expr.Value(get_prop v.Name v.Type attrs, typeof<real>)) m0)
    get_consts m1 |> List.iter(fun (t, n) -> if has_prop n typeof<real> attrs then m1 <- replace_expr (Expr.ValueWithName(0., n)) (Expr.Value(get_prop n typeof<real> attrs, typeof<real>)) m1)
    get_vars m1 |> List.iter(fun v -> if has_prop v.Name v.Type attrs then m1 <- replace_expr (Expr.Var(v)) (Expr.Value(get_prop v.Name v.Type attrs, typeof<real>)) m1)
    
    //get_consts m0 |> List.iter(fun (t, n) -> if has_prop n typeof<real*real> attrs then let s = get_prop n typeof<real*real> attrs :?> real*real in let vv = exprvar2<FunScript.Bindings.JSXGraph.Slider> n in _sliders0.Add(n, [| (fst s); (snd s) |]); m0 <- replace_expr (Expr.ValueWithName(0., n)) (<@ (%vv).Value() @>) m0)
    //get_vars m0 |> List.iter(fun v -> if has_prop v.Name typeof<real*real> attrs then let s = get_prop v.Name typeof<real*real> attrs :?> real*real in _sliders0.Add(v.Name, [| (fst s); (snd s) |]); m0 <- replace_expr (Expr.Var(Var(v.Name, typeof<real>))) (<@ (%s0v).[%exprv(v.Name)].Value() @>) m0)
    //get_consts m1 |> List.iter(fun (t, n) -> if has_prop n typeof<real*real> attrs then let s = get_prop n typeof<real*real> attrs :?> real*real in _sliders1.Add(n, [| (fst s); (snd s) |]); m1 <- replace_expr (Expr.ValueWithName(0., n)) (<@ (%s1v).[%exprv(n)].Value() @>) m1)
    //get_vars m1 |> List.iter(fun v -> if has_prop v.Name typeof<real*real> attrs then let s = get_prop v.Name typeof<real*real> attrs :?> real*real in _sliders1.Add(v.Name, [| (fst s); (snd s) |]); m1 <- replace_expr (Expr.Var(Var(v.Name, typeof<real>))) (<@ (%s1v).[%exprv(v.Name)].Value() @>) m1)
   
    
    //let _sliders0v = Expr.ValueWithName(4, "sliders0v") |> expand_as<int>
    //let __sliders0v = <@ fun () -> %_sliders0v @>
    //let _sliders1v = _sliders1 |> exprv
    
    
    <@
    let grid = {| 
        boundingbox = bbox -0.5 10. 10. -0.5
        showNavigation = true 
        showCopyright = false
        keepAspectRatio = false
        axis = true 
    |}
    let b = board grid |> ignore
    

    
    b

    @> 

let rr = vec2 r (r + s)

draw_v {|r = 6.,7. |} rr

let zz = Expr.Let()
let sys = [
    P + 2 * Q == 4.
    P - Q == 12.
    J == 6.
]

solve sys

let p = ppf [
    P + Q +> 4.
    P == 7.
]


let x, y = realvar2 "x" "y"

let i = indexvar "i"

//<@ fun (a,b) -> a + b @>
//y.[i + 1] == A * y.[i] + 2 |> sexpr
//sys |> List.map sexpr

J == P *** 2 + Q - A

let U1 = utilfun2 (U .= a * x *** 4 + 4 * y *** 3) 

U1.ScalarMapExpr

let rec make_JS_compat    = 
    function
    | SpecificCall <@@ ( ** ) @@> (_, _, [xt; yt])  -> 
        let xxt, yyt = make_JS_compat xt, make_JS_compat yt
        <@@ FunScript.Arithmetic.MathJS.Pow((%%xxt:float), (%%yyt:float)) @@>
    | expr -> traverse expr make_JS_compat

compile <| make_JS_compat U1.ScalarMapExpr


