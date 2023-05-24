// Learn more about F# at http://fsharp.org

open System
open FunScript
open FunScript.Bindings.JSXGraph

open Sylvester

[<EntryPoint>]
let main argv =
    let j = 
        <@
            let board = board {|boundingbox = [|-1; 10; 10; 0|]; showNavigation = true; showCopyright = false; axis = true |}
            //let c = functiongraph (fun x -> x * 2.0 + 1.) 0. 5. {||} board
            let r = [|  ge <| functiongraph (fun x -> x * 2.0 + 1.) 0. 5. {||} |]
            create_geom_elements (board) (r)//r
            
        @>
    printfn "%s" (compile j)
    0 // return an integer exit code
