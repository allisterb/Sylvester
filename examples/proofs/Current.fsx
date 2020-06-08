#load "Include.fsx"

open Sylvester
open PropCalculus
open PredCalculus

let p,q,r,s = var4<bool>
let x,y = var2<bool> 
let p',q',r',s' = <@ p @>, <@ q @>, <@ r @>, <@ s @>
let x' = <@ x @>
let y' = <@ y @>

let P,Q, N, A= var4<bool>
let P',Q', N' = <@ P @>, <@ Q @>, <@ N @>

proof prop_calculus <@ p |&| q ==> (p = q) @> [
    let lemma1 = lemma prop_calculus <@ p |&| q ==> p @> [
        strengthen_and p' q' |> Lemma
    ]  
    let lemma2 = lemma prop_calculus <@ p |&| q ==> q @> [
        commute |> L
        strengthen_and q' p' |> Lemma
    ]
    Deduce lemma1
    Deduce lemma2
    def_true <@ true @> |> Commute |> R
    implies_true <@ p |&| q @> |> Lemma
]