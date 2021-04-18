#load "IncludeMath.fsx"

open FSharp.Quotations

open Sylvester
open PropCalculus
open PredCalculus

let a = var<real>
let rr = Vec<four> <@ [9.; a; 3.; a] @>
rr.IsSymbolic
//Series.geometric_series 1. (0.5) |> take 3

Series.harmonic_series' |> take 4
//let inline infinite_series'' g = g |> (infinite_seq'' >> series)

[<Formula>]
let gg (a:real) n r  = a * r ** real(n - 1)


//let geometric_series (a:int) (r:real) = infinite_series (gg a r)

let geometric_series' a  = infinite_series''<@ gg %a @>

geometric_series' <@ 4. @> |> take 5

let n,N = var2<int>


[<Formula>]
let ff (j:int) = -1.0 ** float j



seq { ff n} |> take 2
//let epsilon = <@ var<real> @> 
let p = proof sequences <@ lim pos_inf (seq {ff n}) = 3. @> [
    let eps = GreekVars.epsilon
    let N' = Engl
    let n' = <@ n @>
    let L' = <@ 3. @>
    def_limit eps N' n' L' <@ ff @> |> LR
] 


//let (!) a = Fn a

let seq_fn f = seq {Fn f}
<@ seq {Fn(fun n -> n + 5).[5]} @> 
ff.[5]
//ss.[5]
Field.R.ToString()
N = Zpos

type R3 = R<dim<4>>

let ff = var<Set<int>>

let a = term<int> "a"
let b = var<real>
let i = var<int>
expand <@ seq {2 ^^ a} @>
let zzz = expand <@ set b true (b * b) (Aleph 1) @>
seq {a.[i]..a.[2]}

let ss = expand <@ (4. |?| set b true (b * b) (Aleph 1)) = exists b true (4. = (b *b ))  @>

match ss with
| SetTheory.SetMember -> true
| _ -> false


let ff = proof prop_calculus <@ seq {a.[i]} :? IConvergent<int> ==>  seq {a.[i]} :? IBounded<int> @> []

let converges = pred<seq<_>>

let def_converges (epsilon:Expr<real>) (N:Expr<int>) (n:Expr<int>) (Li:Expr<real>) (d:Expr<seq<_>>) =      
        def prop_calculus <@ converges %d = forall %epsilon (%epsilon > 0.) (exists %N  (%n > %N)  ((%Li - elem(%d).[%n]) < %epsilon)) @>

let epsilon = var<real>
let L = var<real> 
let n,N,n',N'= var2'<int> "n" "N"

proof prop_calculus <@ converges (seq {1. + 1. / epsilon})  @> [
    def_converges <@ epsilon @> <@ N @> <@ n @> <@ L@> <@ seq {1. + 1. / epsilon} @> |> LR
]

<@ infinite_seq (fun a -> a * 5 / 4) @>
let rr = seq{(5 ^^ a) / i} 
<@Seq [4;5;6]@>
<@ set b (b > 0.) (b * b) (Aleph 0) @> 


expand <@ FSharp.Collections.Set. ]@>
[<Formula>]
let bbb = Seq.initInfinite(fun pp -> pp)

<@ seq{(5 ^^ a) / i} @>

let ggg = <@ Seq.delay<int> (fun () -> Seq.empty) @>
let (|SeqDelay|_|) =
    function
    | Patterns.Call(None, mi0, Patterns.Call(None, mi1, Patterns.Lambda(_, Patterns.Call(None, mi2, v::[]))::[])::[]) when mi0.Name = "CreateSequence" && mi1.Name = "Delay" && mi2.Name = "Singleton" -> Some v
    | _ -> None

match <@ seq{4} @> with
| SeqDelay v -> Some (expand v)
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
