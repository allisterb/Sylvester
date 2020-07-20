namespace Sylvester.Tests.Math

module Set =

    open System
    open System.Linq

    open Xunit

    open Sylvester
    open SetAlgebra

    [<Fact>]
    let ``Can display set symbols``() =
        let A = var<Set<int>>
        let s = Display.print_formula (<@ A = Set.Empty @> |> expand)
        Assert.NotNull s
    
    [<Fact>]
    let ``Algebra has element``() =
        let dice = Seq [1..6]
        let S' = SigmaAlgebra(dice * dice)
        let A = [for i in 1..6 -> (1, i)] 
        Assert.True (S'.Set.HasElement (Seq(A)))
    
    [<Fact>]
    let ``Can prove union commutative``() =
        let A,B = var2<Set<obj>>
        let p = proof set_algebra <@ A |+| B = (A |+| B)@> []
        Assert.True p.Complete
    (*
    [<Fact>]
    let ``Can equate predicates`` () =
        let c = Pred(fun x -> x = 0) 
        let d = Pred(fun x -> x = 0) 
        Assert.Equal(c, d) 

    [<Fact>]
    let ``Can get enumerator``() =
        let s1 = seq {1..6} |> Seq
        Assert.Equal(1, s1.First())
        let s2 = seq {3..5} |> Seq
        Assert.True (s2 |<| s1)

    [<Fact>]
    let ``Can test for subsets``() = 
        let s1 = seq {1..6} |> Seq
        let s2 = seq {3..5} |> Seq
        Assert.True (s2 |<| s1)

        let d = s1 |-| s2
        Assert.Equal(3, d.Length)


    [<Fact>]
    let ``Can get distinct``() = 
        let lt = [|5;6;6;7;8|] |> Set.fromSeq
        let h = lt.Prod
        Assert.Equal(4, h.Length)
          
    [<Fact>]
    let ``ff``() = 
        let x,y = var2<Set<obj>>
        Assert.True(set_algebra |- <@ (x |*| y) = (y |*| x) @>)
      *)