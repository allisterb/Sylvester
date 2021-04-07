#load "IncludeMath.fsx"

open Sylvester
open PropCalculus
open PredCalculus

[<Formula>]
let bounded = 
        let x = elem<real> "x" 
        let epsilon, Ln = var2<real>
        let n,N = var2<int> 
        <@ forall epsilon (epsilon > 0.) (exists N  (n > N)  ((Ln - x.[n]) < epsilon)) @>

<@ bounded @> |> expand
let p = elem<bool> "p"
let q = elem<bool> "q"

1 |?| N
let a = elem<int> "a"
let i = var<int>
let zz  = a * 5
let A = seq {a.[i]}


let n = var<int>
let rr = infinite_seq (fun n -> a.[n+1]) 
let rrr = infinite_seq' <@ fun n a -> a + 1@>

rr.[5]
<@ infinite_seq (fun n -> a.[n])@> |> expand
let inf<'t> = formula<'t>
let ffff = seq a.[0]
<@ sseq a.[0] = sseq [4]  @> |> expand

//seq(a.[0])

let yy =  {a.[0]..a.[i]}
[ 2 * a]
//let x = {a*0..i*a} 
//let rrr = expand x
//<@ %%rrr:SetElement<int> @>
let ff = 4::[]
let x = var<int>
let rr = pred<int>
//let zz = proof prop_calculus <@ x |?| set rr x = forall x (x > 0) (rr x = rr x)@> []

[<Formula>]
let rec f =
    function
    | n when n < 6 -> n + 3
    | n -> f(n - 1)

//let iff = infinite_seq f
<@ infinite_seq f @>

let urn = sseq [1..5] * sseq [1..4]



let S = prob_space urn
let P = prob_measure S

[<Formula>]
let E1 = urn |>| (fun s -> fst s = 5)

let E2 = urn |>| (fun s -> snd s < 4)
let E3 = urn |>| (fun s -> fst s + snd s >= 8)


P(E1 |/| S ) + P(E1)

let ee = var<bool>

expand <@ urn @>
