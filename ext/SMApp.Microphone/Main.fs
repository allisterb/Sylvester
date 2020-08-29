namespace SMApp.Microphone

open WebSharper
open WebSharper.JavaScript
open WebSharper.InterfaceGenerator

module Definition =
    let Mic =
        Class "Mic"
        |+> Instance [
                "connect" => T<string> ^-> T<unit>
                "start" => T<unit> ^-> T<unit>
                "stop" => T<unit> ^-> T<unit>
                "onaudiostart" =@ T<unit> ^-> T<unit> |> WithSourceName "onAudioStart"
                "onaudioend" =@ T<unit> ^-> T<unit> |> WithSourceName "onAudioEnd"
                "onconnecting" =@ T<unit> ^-> T<unit> |> WithSourceName "onConnecting"
                "ondisconnected" =@ T<unit> ^-> T<unit> |> WithSourceName "onDisconnected"
                "onready" =@ T<unit> ^-> T<unit> |> WithSourceName "onReady"
                "onerror" =@ T<string> ^-> T<unit> |> WithSourceName "onError"
                "onresult" =@ (T<obj> * T<obj>) ^-> T<unit> |> WithSourceName "onResult"
            ]
        |+> Static [
                Constructor (T<unit>) |> WithInline "new Wit.Microphone(document.getElementById(\"microphone\"))"
            ]
    
    let Assembly =
        Assembly [
            Namespace "SMApp.Microphone.Resources" [
                Resource "Js" "microphone/js/microphone.min.js"
                |> AssemblyWide
                Resource "Css" "microphone/css/microphone.min.css"
                |> AssemblyWide
            ]
            Namespace "SMApp.Microphone" [
                 Mic
            ]
        ]

[<Sealed>]
type Extension() =
    interface IExtension with
        member ext.Assembly =
            Definition.Assembly

[<assembly: Extension(typeof<Extension>)>]
do ()