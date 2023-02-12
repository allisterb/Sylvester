namespace Sylvester

open System

open FSharp.Quotations

open FunScript
open FunScript.Bindings
open FunScript.Bindings.JSXGraph
open Html

[<RequireQualifiedAccess>]
module Board =
    let draw (src:Expr) =
        let id = Guid.NewGuid().ToString()
        let repbid (s:string) = s.Replace("_bid", id)
        div [
            div [attr "id" id; attr "class" "jxgbox"; attr "style" $"width:1080px;height:200px"]
            script [src |> compile |> repbid |> Text]
        ] |> Html.toString
        