#r "C:\\Users\\Allister\\.nuget\\packages\\fparsec\\1.0.3\\lib\\netstandard1.6\\FParsec.dll"
#r "C:\\Users\\Allister\\.nuget\\packages\\fparsec\\1.0.3\\lib\\netstandard1.6\\FParsecCS.dll"
#r "C:\\Users\\Allister\\.nuget\\packages\\mathnet.numerics\\4.11.0\\lib\\netstandard2.0\MathNet.Numerics.dll"
#r "C:\\Users\\Allister\\.nuget\\packages\\mathnet.numerics.fsharp\\4.11.0\\lib\\netstandard2.0\\MathNet.Numerics.FSharp.dll"
#r "bin\\Debug\\netstandard2.0\\MathNet.Symbolics.dll"
#r "bin\\Debug\\netstandard2.0\\Unquote.dll"
#r "bin\\Debug\\netstandard2.0\\Sylvester.Expressions.dll"

open Sylvester
open FSharp.Quotations
open FSharp.Quotations.Patterns

let var<'t> = Unchecked.defaultof<'t>

let x,y,z = var<float>, var<float>, var<float>

let eqns = <@[ 
    3. * x + 2. * y + 6. * y  + z + 3. * y= 0.
    2. * x - z = 4.
    z - y = 6.
 ]@>

eqns |> expand_list |> List.item 0 |> expand_equality |> fst |> polyn_degree

let polyn_degree (x:Expr) = 
    let x' = expand x in 
    get_vars x'
    |> List.map (fun v -> x' |> MathNetExpr.fromQuotation |> (MathNet.Symbolics.Polynomial.degree (v |> Expr.Var |> MathNetExpr.toIdentifier)))
    |> List.map (MathNetExpr.toQuotation [])
    |> List.map (Option.get)
    |> List.map (fun e -> match e with | Value(v, t) -> v :?> float | _ -> failwith "Unexpected expression in degree.")
    |> List.max
    |> int

polyn_degree <@ 3. * x + 2. * y + 6. * x * x + z@> 

eqns 
|> expand_list 
|> List.map expand_equality 
|> List.map fst 
|> List.map MathNetExpr.fromQuotation
|> List.map (MathNet.Symbolics.Polynomial.degree (MathNetExpr.toIdentifier <@ x @>))
|> List.map (MathNetExpr.toQuotation(eqns |> expand |> get_vars))
|> List.map Option.get
|> List.map (fun e -> match e with | Value(v, t) -> v :?> float | _ -> failwith "Unexpected expression in degree.")


//|> List.map MathNet.Symbolics.Polynomial.degree  
//let eqns_are_linear (x:Expr<bool list>) =
//    let l = x |> ex 
//get_all_eqn_coeffs eqns
