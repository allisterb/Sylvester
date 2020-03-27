namespace Sylvester

[<AutoOpen>]
module Tactics =
    let lemma (l:Theorem) (p:Proof) =
        do if not (sequal l.Expr p.LastState) then failwithf "The last state of the proof is not the first state of the lemma."
        Proof(p.Expr, p.Theory, List.concat [p.Steps; l.Proof.Steps])

