namespace Sylvester

open FSharp.Quotations

open FunScript
open FunScript.Bindings
open FunScript.Bindings.JSXGraph
open Html

module Board =
    let board (src:Expr) (width:string) (height:string) (id:string) =
        div [
            div [attr "id" id; attr "class" "jxgbox"; attr "style" $"width:{width};height:{height}"]
            script [attr  "type" "text/javascript"; Text(compile src)]
        ] |> Html.toString
        