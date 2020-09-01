namespace Sylvester.Web.Thea

open WebSharper
open WebSharper.JavaScript
open WebSharper.Web

open WebSharper.Sitelets
open WebSharper.UI
open WebSharper.UI.Html
open WebSharper.UI.Server

open SMApp.Bootstrap

type Route =
    | [<EndPoint "/">] Home
    | [<EndPoint "/about">] About

module Templates =
    type MainTemplate = Templating.Template<"wwwroot/Main.html">

    let Main ctx action (title: string) (body: Doc list) =
        Content.Page(
            MainTemplate()
                .Title(title)
                .Body(body)
                .Doc()
        )
    
module Site =
    let HomePage ctx =
        Templates.Main ctx Home "Home" [
            div [attr.id "main"; attr.``class`` "container"] [
                client <@ Client.run() @>                
            ]
        ]

    let AboutPage ctx =
        Templates.Main ctx About "About" [
            h1 [] [text "About"]
            p [] [text "This is a template WebSharper client-server application."]
        ]
            
    [<Website>]
    let Main = 
        Sitelet.Infer <| fun ctx route ->
            match route with
            | Home -> HomePage ctx
            | About -> AboutPage ctx