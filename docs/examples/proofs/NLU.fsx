#load "Include.fsx"

open Sylvester
open Sylvester.NLU

#r ".\\..\\..\\src\\Lang\\Sylvester.Prover\\bin\\Debug\\netstandard2.0\\Sylvester.Runtime.dll"
#r ".\\..\\..\\src\\Lang\\Sylvester.Prover\\bin\\Debug\\netstandard2.0\\Sylvester.NLU.Wit.dll"

let wc = getIntent "DD"

//<@ p ||| (q |&| r) = r @> |> src
src (ExprParser.parse "(p or (q and r)) = r")
sequal (ExprParser.parse "(p or (q and r)) = r") (expand <@ (p ||| (q |&| r)) = r @>)