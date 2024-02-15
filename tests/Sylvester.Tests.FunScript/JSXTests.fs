namespace Sylvester.Tests.FunScript

open System
open Xunit

open FunScript
open FunScript.Bindings
open FunScript.Bindings.JSXGraph

open Sylvester

type Foo = {
    Bar: float[] option
    Baz: string option
}

module JSXGraphTests = 
    [<Fact>]
    let ``can create fun``() =
       let f = <@ fun x -> x + 2. @>
       let ss = compile <@ (%f)(3.) @>
       Assert.NotNull ss
    
    
    [<Fact>]
    let ``can create obj``() =
       let ss = compile <@ {|origin = 2; length = 1 |} @>
       Assert.NotNull ss
    
    
    [<Fact>]
    let ``can create``() =
        let ss = 
            compile <@ 
                    let b = JXG.JSXGraph.initBoard("foo", {|boundingbox=[|4;5;6;7|]; show = false |}) 
                    b.hasPointerUp = true
            @>
        Assert.NotNull ss

    [<Fact>]
    let ``can create polygon``() =
        let d = 
            compile <@
                        let board = JXG.JSXGraph.initBoard("jxgbox", {|boundingbox = [|-5; 2; 5; -2|];
                                                    keepAspectRatio =true; showCopyright = false; showNavigation =false |})
        
                        let p = board.createPoint([|-2.; 0.|], {| name = "A" |})
                        let q = board.createPoint([|-1.; -1.|], {| name = "B" |})
                        let r = board.createPoint([|1.0; -0.5|], {| name = "C" |})
                        let s = board.createPoint([|1.; 1.|], {| name = "D" |})
                        let t = board.createPoint([|-1.; 1.5|], {| name = "E" |})

                        let poly1 = board.createPolygon([|p; q; r; s; t|], {| name = "Polygon 1"; withLabel = true |});
                        poly1
            @>
        Assert.NotNull d

    [<Fact>]
    let ``can create board``() =
        let b =
            <@
            let board = JXG.JSXGraph.initBoard("jxgbox", {|boundingbox = [|-5; 2; 5; -2|];
                                        keepAspectRatio =true; showCopyright = false; showNavigation =false |})
        
            let p = board.createPoint([|-2.; 0.|], {| name = "A" |})
            let q = board.createPoint([|-1.; -1.|], {| name = "B" |})
            let r = board.createPoint([|1.0; -0.5|], {| name = "C" |})
            let s = board.createPoint([|1.; 1.|], {| name = "D" |})
            let t = board.createPoint([|-1.; 1.5|], {| name = "E" |})

            let poly1 = board.createPolygon([|p; q; r; s; t|], {| name = "Polygon 1"; withLabel = true |});
            poly1
            @> |> draw_board
        Assert.NotNull b

    [<Fact>]
    let ``can create point``() =
        let code = 
            <@ 
                let board = create_board {| boundingbox = [|-5; 2; 5; -2|]; keepAspectRatio =true; showCopyright = false; showNavigation =false |}
                create_point board [|-2.; 0.|] {||}
            @>
        let s = code |> draw_board |> Html.toString
        Assert.NotNull s
        
        
    [<Fact>]
       let ``can set board options``() =
           let code = 
               <@ 
                   let board = create_board {| boundingbox = [|-5; 2; 5; -2|]; keepAspectRatio =true; showCopyright = false; showNavigation =false |}
                   board.options.label.autoPosition <- true
               @>
           let s = code |> draw_board |> Html.toString
           Assert.NotNull s
           