#load "Include.fsx"

open Sylvester
open PropCalculus

/// true = (p = p)
let p,q,r = var3<bool>

// Theorem 3.17
 
    //let lemma1 = id_ax prop_calculus <@ (p <> q) = not (p = q)@> 
    //let lemma2 = id_ax prop_calculus <@ (q <> r) = not (q = r)@>
    //let lemma3 = id_ax prop_calculus <@ (not (p = q) <> r) = not (not (p = q) = r)@>
    //let lemma4 = id_ax prop_calculus <@ p <> not (q = r) = not (p = not (q = r)) @>
    //let lemma5 = id_ax prop_calculus 

proof prop_calculus <@ ((p <> q) <> r) = (p <> (q <> r)) @> [
        DefNotEquiv <@ p @> <@ q @> |> L
        DefNotEquiv <@ not (p = q) @> <@ r @> |> L
        DefNotEquiv <@ q @> <@ r @> |> R
        DefNotEquiv <@ p @> <@ not (q = r) @> |> R
        DistribNot <@ q @> <@ r @> |> R
        //R lemma2
        //L lemma3
        //R lemma4
        //L Distrib
        //R Distrib
        //L Distrib
        //L Distrib
        //DoubleNegation <@ p @> |> L
        //R Distrib
        //R LeftAssoc
    ]

