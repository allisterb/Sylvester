namespace Sylph

/// Additional theorems of S useful in proofs.
module S =
    // not p = q == p == not q
    let NotEquivSymmetry = 
        let t = <@fun p q -> not p = q = p = not q @> |> theorem S [
            Collect' |> LeftA
            RightAssoc' |> EntireA
            Commute' |> RightA
            Collect' |> RightA
            Commute' |> RightA
        ] 
        Lemma t
    
    // not not p == p
    let DoubleNegation = 
        let t = <@ fun p q -> (p <> q) = not p = q @> |> logical_theorem [
                RightAssoc' |> EntireA
                Collect' |> EntireA
            ]
        Lemma t
