module Tests

open System
open Xunit

open FunScript
open FunScript.Bindings

[<ReflectedDefinition>]
module Foo =   
    // Create a function that will be compiled into JavaScript...
    let htmlCanvasTS()=
        let canvas = Globals.document.getElementsByTagName_canvas().[0]
        canvas.width <- 1000.
        canvas.height <- 800.
        let ctx = canvas.getContext_2d()
    
        ctx.fillStyle <- "rgb(200,0,0)"
        ctx.fillRect (10., 10., 55., 50.);
        ctx.fillStyle <- "rgba(0, 0, 200, 0.5)"
        ctx.fillRect (30., 30., 55., 50.)


[<Fact>]
let ``My test`` () =
    // Create a function that will be compiled into JavaScript...
    let htmlCanvasCode =
        Compiler.Compiler.Compile(
            // This argument is the quotation to compile
            <@ Foo.htmlCanvasTS() @>, 
            // This argument tells the compiler not to wrap 
            // the result in a return statement
            noReturn=true)
    Assert.NotNull htmlCanvasCode
