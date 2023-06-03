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
            let p = point 5. 10. defaults board
            let q = point 3. 1. defaults board
            
            draw_ge board [|  
                ge <| functiongraph (fun x -> x * 2.0 + 1.) 0. 5. defaults 
                
                ge <| line p q defaults
            |]//r
            
        @>
    printfn "%s" (compile j)
    printfn "%s" (compile <@ let x = tuple_to_array (4, 5) in x.[0] @>)
    0 // return an integer exit code
