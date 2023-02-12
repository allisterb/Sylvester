namespace Sylvester

open System

open FSharp.Quotations

open FunScript
open FunScript.Bindings
open FunScript.Bindings.JSXGraph
open Html

[<AutoOpen>]
module Board =
    let draw_board (src:Expr) =
        let id = Guid.NewGuid().ToString()
        let repbid (s:string) = s.Replace("_bid", id)
        div [
            div [attr "id" id; attr "class" "jxgbox"; attr "style" $"width:768px;height:200px"]
            script [src |> compile |> repbid |> Text]
        ]
        
    [<Emit("JXG.JSXGraph.initBoard(\"_bid\", {0})")>]
    let create_board (attr:obj) = stub<Board>
        