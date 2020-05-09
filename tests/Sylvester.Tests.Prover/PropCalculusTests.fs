namespace Sylvester.Tests.Prover

module PropCalculusTests =    
    open Xunit
    
    open Sylvester
    open PropCalculus
    
    [<Fact>]
    let ``operator works``() =
        let p,q,r = var3<bool>
        // Theorem 3.31
        let ``3.31`` = proof prop_calculus <@ (p ||| (q ||| r)) = ((p ||| q) ||| (p ||| r)) @> [
            idemp_or <@ p @> |> Trn |> L
        ]
        let z = proof prop_calculus <@ false = (not p = p) @> [
            R Collect
            DefTrue |> R |> LR'
        ] 
        Assert.NotNull z
        Assert.True(prop_calculus |- <@ p = q = q = p @>)
        Assert.NotNull(axiom prop_calculus <@ true @>)
        // Theorem 3.2
        let p,q,r = var3<bool>
        let ``3.2`` =
            let lemma1 = ident prop_calculus <@ (p = (q = q)) = p @> [
                L LeftAssoc 
                LR RightAssoc
            ]
        
            theorem prop_calculus <@ p = p = q = q @>  [
                LR RightAssoc
                LR RightAssoc
                R lemma1
            ]
        Assert.NotNull ``3.2``
