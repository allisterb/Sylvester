namespace Sylvester.Tests.Math

module Set =

    open System
    open System.Linq

    open Xunit

    open Sylvester
    open Sylph
    open SetAlgebraTheory

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
        
        //let l1 = ident_axiom set_algebra <@y = y@> |> Lemma |> EntireA
        
        let t_3_1 = <@(x |+| y = U) |&| (x |*| y = Set.Empty) ==> (y = x)@> |> theorem set_algebra []
        
        let l1 = ident_axiom set_algebra <@(y |+| Set.Empty) = y@>
        
        
        Assert.NotNull t_3_1