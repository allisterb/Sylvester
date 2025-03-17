namespace Sylvester.Tests


module Matrix = 
    open Xunit

    open Sylvester
    open Matrix

    //do CAS.Maxima.init "C:\\MathTools\\maxima-5.44.0\\bin\\maxima.bat"
    
    let x,y,z = realvar3 "x" "y" "z"

    [<Fact>]
    let ``Can construct matrices`` () =
        let m1 = mat [
            [x; y] 
            [2; 4]
        ]
        Assert.NotNull m1
        ()

    [<Fact>]
    let ``Can find charpoly``() =
               
        do CAS.Maxima.init "C:\\MathTools\\maxima-5.44.0\\bin\\maxima.bat"
        let l = realvar "l"
               
        let A = sqmat [1;2;-1;4;-1;3;0;2;2;1;1;2;1;4;1;3]
               
               
        let r = mcharpoly l A
        Assert.NotNull r
    
    
    [<Fact>]
    let ``Can find echelon``() =
        
        do CAS.Maxima.init "C:\\MathTools\\maxima-5.44.0\\bin\\maxima.bat"
        let l = realvar "l"
        
        let A = sqmat [1;2;-1;4;-1;3;0;2;2;1;1;2;1;4;1;3]
        
        
        let r = mechelon A
        Assert.NotNull r

        

    [<Fact>]
    let ``Can multiply``() =
        do CAS.Maxima.init "C:\\MathTools\\maxima-5.44.0\\bin\\maxima.bat"
        let A = sqmat [2;0;1;-3;0;2;10;4;0;0;2;0;0;0;0;3]
        let J = jordan_normal_form A |> perm_jordan_blocks [1;0;2]
        //J |> jordan_blocks |> Array.map(fun b -> b.[0,0].Expr, exprv b.Dims.[0])  |> Array.map(fun (e,n) -> sprintf "[%s,%s]" (sprinte e) (sprinte n)) |> Array.reduce (sprintf "%s,%s") |> sprintf "[%s]"
        let P = jordan_similar J A 
        Assert.NotNull (P * P)
        