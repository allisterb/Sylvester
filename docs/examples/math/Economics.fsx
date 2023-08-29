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
open MicroEconomics
open LinearEquations
open FunScript
let get_props (o:'a) = typeof<'a>.GetProperties()
   

let (__) (a:real) (b:real) = a,b
let rr = 0.6 .... 0.7
let draw (o:'a) =
    let p = get_props o
    p

draw {|x="foo"; y = 5|} 
//typeof<Vec<dim<2>>>.GetMethod("get_Item")
do Maxima.init "C:\\MathTools\\maxima-5.44.0\\bin\\maxima.bat"
let P, Q, J = realvar "P", realvar "Q", realvar "J"
let U = realvar "U"
let A = realconst "A"
//P + Q == 5. |> sexpr
let Qs = demand (P .= 2 * Q + 4 + A)

Qs.[A]

 
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

let a, b = realconst2 "a" "b"
let x, y = realvar2 "x" "y"

let i = indexvar "i"

//<@ fun (a,b) -> a + b @>
//y.[i + 1] == A * y.[i] + 2 |> sexpr
//sys |> List.map sexpr

J == (P ^^ 2) + Q - A

let U1 = utilfun2 (U .= a * (x ^^ 4) + 4 * (y ^^ 3)) 

U1.ScalarMapExpr

let rec make_JS_compat    = 
    function
    | SpecificCall <@@ ( ** ) @@> (_, _, [xt; yt])  -> 
        let xxt, yyt = make_JS_compat xt, make_JS_compat yt
        <@@ FunScript.Arithmetic.MathJS.Pow((%%xxt:float), (%%yyt:float)) @@>
    | expr -> traverse expr make_JS_compat

compile <| make_JS_compat U1.ScalarMapExpr

type RealFunctionDiagram = {
    xmin:float
    xmax:float
}

let (|HasProp|_|) (name:string) (t:Type) :'t->obj option =
    function
    | o when typeof<'t>.GetProperties().Any(fun p -> p.Name = name && p.PropertyType = t) -> Some (typeof<'t>.GetProperties().First(fun p -> p.Name = name).GetValue(o))
    | _ -> None

let has_prop<'a> n t (x:'a) = typeof<'a>.GetProperties().Any(fun p -> p.Name = n && p.PropertyType = t)
   
let get_prop<'a> n t (o:'a) = typeof<'a>.GetProperties().First(fun p -> p.Name = n && p.PropertyType = t).GetValue(o)

let get_consts expr =
       let dict = new System.Collections.Generic.List<Type*string>()
       expr |> traverse' (function | ValueWithName(_, t, n) -> dict.Add(t, n); None | _ -> None) |> ignore
       dict |> List.ofSeq

let draw_f(attrs:'a) (f:RealFunction) = 
    let _attrs = new Dictionary<string, obj>()
    let tt = typeof<int>
    if (has_prop "xmax" typeof<real> attrs) then _attrs.Add("xmax", get_prop "xmax" typeof<real> attrs)
    ()


let cc = {|x="foo"; a = 6.|}
has_prop "Y" typeof<ScalarConst<int>> cc

let mutable m = U1.ScalarMapExpr.Raw

get_consts U1.ScalarMapExpr

//get_prop "a" typeof<real> cc
get_consts U1.ScalarMapExpr |> List.iter(fun (t, n) -> if has_prop n typeof<real> cc then m <- replace_expr (Expr.ValueWithName(0., n)) (Expr.Value(get_prop n typeof<real> cc, typeof<real>)) m)
compile <| make_JS_compat m
