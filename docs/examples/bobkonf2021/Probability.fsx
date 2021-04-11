#load "IncludeMath.fsx"

open FSharp.Quotations

// bounded<float> (seq (Seq.delay (fun () -> Seq.singleton (a.Item(i)))))
open Sylvester
open PropCalculus
open PredCalculus
open Sequences

let a = term<int> "a"
let b = var<real>
let i,n = var2<int>
<@ seq{(5 ^^ a) / i} @>
let rr = seq{(5 ^^ a) / i} 

[<Formula>]
let bbb = Seq.initInfinite(fun pp -> pp)

<@ seq{(5 ^^ a) / i} @>

let ggg = <@ Seq.delay<int> (fun () -> Seq.empty) @>
let (|SeqDelay|_|) =
    function
    | Patterns.Call(None, mi0, Patterns.Call(None, mi1, Patterns.Lambda(_, Patterns.Call(None, mi2, v::[]))::[])::[]) when mi0.Name = "CreateSequence" && mi1.Name = "Delay" && mi2.Name = "Singleton" -> Some v
    | _ -> None

match <@ seq{(5 ^^ a) / i} @> with
| SeqDelay v -> Some v
| _ -> None


let cc = seq {b**b}
<@ lim pos_inf (seq {1 + 1 / i}) = 1  @> 

src <@ a @>
let A = seq {a.[i]}
//a.[i]
//seq {1..}
//let oo = <@[4.;7.; b; 1.]@>
//expand_list oo

let converges = pred<seq<_>>

let def_converges (epsilon:Expr<real>) (N:Expr<int>) (n:Expr<int>) (Li:Expr<real>) (d:Expr<seq<_>>) =      
        def prop_calculus <@ converges %d = forall %epsilon (%epsilon > 0.) (exists %N  (%n > %N)  ((%Li - elem(%d).[%n]) < %epsilon)) @>

let epsilon = var<real>
let L = var<real> 
let n,N,n',N'= var2'<int> "n" "N"

proof prop_calculus <@ lim pos_inf (seq {1 + 1 / i})  = 1@> [
    //def_converges <@ epsilon @> <@ N @> <@ n @> <@ L@> <@ seq {1. + 1. / L} @> |> LR
]


expand (bounded_def <@ seq {1.0..2.0} @>)

//let p = elem<bool> "p"
//let q = elem<bool> "q"

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
