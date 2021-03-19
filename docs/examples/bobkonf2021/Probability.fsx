#load "IncludeMath.fsx"

open Sylvester
let subsets xs = List.foldBack (fun x rest -> rest @ List.map (fun ys -> x::ys) rest) xs [[]]
let subsets' (xs:seq<'t> when 't : equality) = 
                Seq.foldBack (fun x rest -> Seq.append rest (Seq.map (fun ys -> (Seq.append (seq {yield x}) ys)) rest)) xs (seq {yield Seq.empty})
                |> Seq.map(fun s -> if Seq.isEmpty s then Empty else Seq(s))
                |> Set.fromSeq

let j = Seq [1;2;3;4;5;6]
let zz = subsets' (j * j)

zz.[560] |> Seq.toList

let zzz = subsets' j 


//seq {for i in 0 .. 31 do yield let s = Seq.item i zzz in if Seq.isEmpty s then Empty else Seq(s)} |> Set.ofSeq
//(Seq.item 0 rrr) |> Seq.item 


dice.Length
let S = prob_space (dice * dice)
let P = S.Measure
//let comp = S.Set.Difference
//let A = sseq2 [1..3]


A.Length
P(A)


let dd = dice * dice

let S = ProbabilitySpace (dd)

//Seq.length <| { for i in 1 .. 10 -> i * i }
//dice.[2u]

let S = ProbabilitySpace (dice * dice)
let P = S.Measure
let comp = S.Set.Difference
let A = sseq2 [5..6]
P(A)

A |+| (dice *  dice) |> seq_length

open FSharp.Quotations
type TS =
| Foo of Expr<int>

let 
let p = set 
p