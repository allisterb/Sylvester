#load "IncludeMath.fsx"

open Sylvester
open Vector
open Dimension



let x, y = var2'<int> "x" "y"
let f(x, y) = 0 

sprint' <@ f(%x, %y)  = f(%y, %x) @>

let inline (|HasId|) (n:int) x = (^a : (member Id : 'Id)x) // define type class
let inline getId (HasId 4 x) = x // call type class instance

type X() =
    member val Id = 1
    
let x = X()

getId x

[<Formula>]
let g x y z = x ** 2. + 3. * x + z ** 6. + y

let g' = diff <@ g @> (realvar "z") |> ev

g' 4. 5. 6.
match <@ fun x -> x + 2 @> with
| FSharp.Quotations.Patterns.Lambda(v, Quotations.Patterns.Call(_,_,Quotations.Patterns.Var(vv)::_::[])) -> vv.ToString()
| _ -> ""
[<Formula>]
let f x y z = x + z ** 2. + y

let f' = %%% diff //<@ f @> (realvar "z")


let rr = diff <@ f @>
foo <@ (fun x y z -> 0) @>

foo <@ (fun x y -> 0) @>

let x, y = realvar "x", realvar "y" 

sum x x <@ 1 @> <@ 5 @>
definite_integral <@ fun a -> a ** 2.  @> 0. 2.


let n = realvar "n"

[<Formula>]
let F x y = Vec<``2``> <@[x + 2.; y]@> 

vexpr (F 1. 1.)
//

//let x = vec ``3`` <@ 1. / %n, 1. / %n, 2. / %n @>

norm x

