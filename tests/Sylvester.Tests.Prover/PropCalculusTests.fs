namespace Sylvester.Tests.Prover

open Sylvester 

module PropCalculusTests =    
    open Xunit
    
    open Sylvester
    open PropCalculus
    open PredCalculus

    module Vars = 
        let P,Q,N,S = var4<bool>
        let x,y = var2<bool>

    
    open Vars
    [<Fact>]
    let ``operator works``() =
        
        let pp = proof pred_calculus <@ (forall' x P) ==> P @> [
            Instantiate pred_calculus <@ forall' x P @> <@ x @> <@ P @> |> L
            //axiom pred_calculus <@ P ==> P @> |> Deduce
            //ident_forall_true' x' |> R
        ]
        Assert.NotNull pp
        (*

        let pp = proof pred_calculus <@ exists x N (forall y Q P) ==> (forall y Q (exists x N P)) @> [
            def_implies |> LR
            distrib_or_forall |> L 
        ]
        Assert.NotNull pp
        *)
        //Assert.NotEmpty (Patterns.get_quantifiers <@ exists x N P @>)
        //let p,q,r,s = var4<bool> 
        //let p',q',r',s' = <@ p @>, <@ q @>, <@ r @>, <@ s @>
        //let x,i,j,k = var4<int>
        //let p1 = proof prop_calculus <@ forall (i,j) false ==> (i + j > 0) @> [] 
        //Assert.NotNull p1
        //let g = <@ sum (x) (x = 3) (x * x) = 9 @>
        //Assert.True (Theory.S |- g)
        //let x = (!!) [i, j, k] (i > 0)  (i + j > 5) 
        //let z = x ||| p
        //<@ z @> |> expand 
        //let t = (!?)  p' <@ p 
        //p |&| x
        
        //[<Formula>]
        //let x = (!!) [i, j, k] (i > 0) (i + j > 5)
        
        //let z = <@ x.y @>
       
        (**
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
        *)