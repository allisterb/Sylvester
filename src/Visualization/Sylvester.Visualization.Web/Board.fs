namespace Sylvester

open FSharp.Quotations

open FunScript
open FunScript.Bindings
open FunScript.Bindings.JSXGraph
open Html

[<RequireQualifiedAccess>]
module Board =
    let draw (id:string) (width:int) (height:int) (src:Expr) =
        div [
            div [attr "id" id; attr "class" "jxgbox"; attr "style" $"width:{width}px;height:{height}px"]
            script [src |> compile |> Text]
        ] |> Html.toString
        