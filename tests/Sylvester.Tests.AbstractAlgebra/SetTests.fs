namespace Sylvester.Tests.Math

module Set =

    open System
    open System.Linq
    open FSharp.Quotations
    open Xunit

    open Sylvester
    open SetAlgebra

    [<Fact>]
    let ``Can getZ``() =
        
        let dice = finite_seq [1..6]
        
        let p = (dice * dice).Powerset
        Assert.NotEmpty p

    (*
    [<Fact>]
    let ``Can has element term``() =
        let A = SetTerm<int> ((Expr.Var(Var.Global("A", typeof<Set<int>>))) |> expand''<Set<int>>)
        
        let x = Expr.Var(Var.Global("x", typeof<int>)) |> expand''<int> |> Term<int>
        
        
        Assert.NotNull (x |?| (A))
    
    [<Fact>]
    let ``Can display set symbols``() =
        let n,t = var2<int>
        let A,B = var2<Set<obj>>
        let se = var<seq<int>>

        //Z |> Seq.take 4 |> Seq.toArray

        let Y = SetComprehension(t > 5, t * 3) |> Set
        //let pp = proof set_algebra <@ forall n (%%Y.Range n) (n = (%%Y.Body n)) @> []

        Assert.NotNull (Display.print_formula <@ forall n (%%Y.Range n) (n = (%%Y.Body n)) @>)
    
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
    
    [<Fact>]
    let ``Can get set bodt``() =
        let n = var<int>
        let se = var<seq<int>>
        ()
        //let SS = Gen((Seq.initInfinite (fun x -> x + 5)), fun _ _ -> true) |> Set.fromGen
        //Assert.NotNull (SS.Body n)
        
    [<Fact>]
    let ``set algebra``() =
        let A = var<SetFamily<int>>
        let i, n = var2<int>
        
        let dd = proof set_algebra <@ (intersect i (i < n) A.[i]) = Empty@> []
        
        let ddd = Display.print_formula (expand <@ (intersect i (i < n) A.[i]) @>)
        Assert.NotNull ddd
    *)
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