namespace Sylvester

[<AutoOpen>]
module Tactics =
    let with_lemma (l:Theorem) (p:Proof) =
        do if not (sequal l.Stmt p.LastState) then failwithf "The last state of the proof is not the first state of the lemma."
        do p.Msg (sprintf "Completing proof with lemma \u22A2 %s." (src l.Stmt))
        Proof(p.Expr, p.Theory, List.concat [p.Steps; l.Proof.Steps])