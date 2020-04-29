#load "Include.fsx"

open Sylvester
open IntegerAlgebra 

// Declare some integer variables for use in formulae.
let a,b,c = var3<int>

// Prove the identity a * 0 = 0 use the rules and axioms of integer algebra.
let p1 = proof integer_algebra <@ a * 0 = 0 @>  [
    // a * 0 = a * 0 + 0 is axiomatic in the integer_algebra theory.
    let lemma1 = int_id <@ (a * 0) = (a * 0 + 0) @> [LR Commute] 
    
    // 0 = -(a * 0 ) + (a * 0) can be proved in the integer_algebra theory.
    let lemma2 = int_id <@ 0 = -(a * 0) + (a * 0) @>  [Commute |> R]
    
    // Substitute the identity in lemma1 into A
    lemma1 |> L
    
    // A is commutative
    Commute |> L
    // Subsititute the identity in lemma2 into the left of A
    lemma2 |> L    
    // Subsititute the identity in lemma2 into B
    lemma2 |> R
    RightAssoc |> L
    LeftCancel |> LR
    Collect |> LR
    Reduce |> L
]