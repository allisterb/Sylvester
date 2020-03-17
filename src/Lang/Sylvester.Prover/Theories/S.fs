namespace Sylph

open FSharp.Quotations
/// Additional theorems of S useful in proofs.
module S =
        
    /// not p = q = p = not q
    let NotEquivSymmetry (p:Expr<bool>) (q:Expr<bool>) = 
        let t = <@ not %p = %q = %p = not %q @> |> theorem S [
            Collect' |> LeftA
            RightAssoc' |> EntireA
            Commute' |> RightA
            Collect' |> RightA
            Commute' |> RightA
        ] 
        Lemma t
    
    // not not p == p
    let DoubleNegation (p:Expr<bool>) (q:Expr<bool>) = 
        let t = <@ (%p <> %q) = not %p = %q @> |> logical_theorem [
                RightAssoc' |> EntireA
                Collect' |> EntireA
            ]
        Lemma t
