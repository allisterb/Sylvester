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
        