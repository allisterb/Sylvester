namespace SMApp.Bootstrap

open WebSharper
open WebSharper.UI
open WebSharper.UI.Html
open WebSharper.UI.Client
open WebSharper.JQuery

module Resources =
    open WebSharper.Core.Resources
    
    type CSS() =
        inherit BaseResource("https://stackpath.bootstrapcdn.com/bootstrap/4.5.2/css", "bootstrap.min.css")
    type PopperJS() =
        inherit BaseResource("https://cdn.jsdelivr.net/npm/popper.js@1.16.1/dist/umd", "popper.min.js")
    type JS() =
        inherit BaseResource("https://stackpath.bootstrapcdn.com/bootstrap/4.5.2/js", "bootstrap.min.js")

[<Require(typeof<Resources.CSS>);Require(typeof<JQuery.Resources.JQuery>);Require(typeof<Resources.PopperJS>);Require(typeof<Resources.JS>)>]
[<JavaScript>]
module Controls =
    
    let private cls = Attr.Class

    let Class = Attr.Class

    let Container c = div (cls "container"::[]) c
    
    let Input lbl extras (target, labelExtras, targetExtras) =
        div (cls "form-group" :: extras) [
            label labelExtras [text lbl]
            Doc.Input (cls "form-control" :: targetExtras) target
        ]

    let InputPassword lbl extras (target, labelExtras, targetExtras) =
        div (cls "form-group" :: extras) [
            label labelExtras [text lbl]
            Doc.PasswordBox (cls "form-control" :: targetExtras) target
        ]

    let TextArea lbl extras (target, labelExtras, targetExtras) =
        div (cls "form-group" :: extras) [
            label labelExtras [text lbl]
            Doc.InputArea (cls "form-control" :: targetExtras) target
        ]

    let Checkbox lbl extras (target, labelExtras, targetExtras) =
        div (cls "checkbox" :: extras) [
            label labelExtras [
                Doc.CheckBox targetExtras target
                text lbl
            ]
        ]

    let Radio lbl extras (target, labelExtras, targetExtras) =
        div (cls "radio" :: extras) [
            label labelExtras [
                Doc.Radio targetExtras true target
                text lbl
            ]
        ]