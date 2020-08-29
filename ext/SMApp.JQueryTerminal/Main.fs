// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2018 IntelliFactory
//
// Licensed under the Apache License, Version 2.0 (the "License"); you
// may not use this file except in compliance with the License.  You may
// obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied.  See the License for the specific language governing
// permissions and limitations under the License.
//
// $end{copyright}
namespace WebSharper.JQueryTerminal

open WebSharper
open WebSharper.InterfaceGenerator

module Definition =
    let Terminal = Class "Terminal"

    let callbackSA = T<string[]> ^-> T<unit>
    let callbackS = T<string> ^-> T<unit>
    let interpreterFunction =  Terminal-*T<string>^-> T<unit>
    let greetingsCallback = callbackS ^-> T<unit>

    let Options =
        Pattern.Config "Options"{
            Required=[]
            Optional=
            [
                "checkArity", T<bool>
                "clear", T<bool>
                "clickTimeout", T<int>
                "completion", T<string[]>
                "convertLinks", T<bool>
                "describe", T<string> + T<bool>
                "echoCommand", T<bool>
                "enabled", T<bool>
                "exceptionHandler", callbackS
                "exechash", T<bool>
                "exit", T<bool>
                "extra", T<obj>
                "greetings", T<string> // + greetingsCallback
                "history", T<bool>
                "historyFilter", T<string> ^-> T<bool>
                "historySize", T<int>
                "historyState", T<bool>
                "importHistory", T<bool>
                //"keydown", (Event<Key> * Terminal) ^-> T<bool>
                "keymap", T<obj>
                //"keypress", T<>
                "linksNoReferer", T<bool>
                "linksNoReferrer", T<bool>
                //"login", T<> nope
                "maskChar", T<bool> + T<string>
                "memory", T<bool>
                "name", T<string>
                "numChars", T<int>
                "numRows", T<int>
                "onAfterCommand", T<string> ^-> T<unit>
                //"onAfterLogout", T<>
                //"onAjaxError"
                "onBeforeCommand", T<string> ^-> T<unit>
                //"onBeforeLogin"
                //"onBeforeLogout"
                "onBlur", Terminal ^-> T<bool>
                //"onClear"
                //"onCommandChange"
                //"onCommandNotFound"
                //"onExit"
                //"onExport"
                //"onImport"
                //"onFocus"
                "onInit", Terminal ^-> T<unit>
                "onPause", T<unit> ^-> T<unit>
                "onPop", T<unit> ^-> T<unit>
                "onPush", T<unit> ^-> T<unit>
                "onRPCError", T<string> ^-> T<unit>
                "onResize", T<unit> ^-> T<unit>
                "onResume", T<unit> ^-> T<unit>
                //"onTerminalChange"
                "outputLimit", T<int>
                "pauseEvents", T<bool>
                "processArguments", T<bool> + (T<string> ^-> T<string[]>)
                "processRCPResponse", T<obj> ^-> T<unit>
                "prompt", T<string> //, T<>
                //"request", T<>
                //"response", T<>
                "scrollBottomOffset", T<int>
                "scrollOnEcho", T<bool>
                "softPause", T<bool>
                "wordAutocomplete", T<bool>
                "wrap", T<bool>
            ]
        }
    
    let EchoOptions =
        Pattern.Config "EchoOptions"{
            Required = []
            Optional =
            [
                "raw", T<bool>
                "flush", T<bool>
                "keepWords", T<bool>
            ]
        }

    let InterpreterOptions =
        Pattern.Config "InterpreterOptions"{
            Required = []
            Optional =
            [
                "name", T<string>
                "prompt", T<string>
                //onExit
                //onStart
                //keydown
                "completion", T<string[]>
            ]
        }

    Terminal
        |+> Static[
            Constructor(T<string>?target * interpreterFunction?interpreter * Options?options)
            |>WithInline("$($target).terminal($interpreter, $options);")
        ]
        |+> Instance[
            "clear" => T<unit> ^-> T<unit>
            "destroy" => T<unit> ^-> T<unit>
            "echo" => (T<string> + (T<string> ^-> T<string>)) * !? EchoOptions ^-> T<unit>
            "echoHtml" => T<string>?s ^-> T<unit>
            |>WithInline("$this.echo($s, {raw:true})")
            "enable" => T<unit> ^-> T<unit>
            "focus" => T<bool> ^->T<unit>
            "disable" => T<unit> ^-> T<unit>
            "flush" => T<unit> ^-> T<unit>
            "settings" => T<unit> ^-> Options
            "scroll_to_bottom" => T<unit> ^-> T<unit>
            |>WithSourceName "ScrollToBottom"
            "pause" => (T<bool> + T<unit>) ^-> T<unit>
            "resume" => T<unit> ^-> T<unit>
            "reset" => T<unit> ^-> T<unit>
            "paused" => T<unit> ^-> T<bool>
            "push" => (interpreterFunction * Options) ^-> T<unit>
            "pop" => T<unit> ^-> T<unit>
            "set_prompt" => T<string>^->T<unit> |> WithSourceName "setPrompt"
        ]|>ignore

    let Assembly =
        Assembly [
            Namespace "SMApp.JQueryTerminal.Resources" [
                Resource "Js" "https://cdnjs.cloudflare.com/ajax/libs/jquery.terminal/2.17.6/js/jquery.terminal.min.js"
                |> RequiresExternal [T<WebSharper.JQuery.Resources.JQuery>]
                |> AssemblyWide
                Resource "Css" "https://cdnjs.cloudflare.com/ajax/libs/jquery.terminal/2.17.6/css/jquery.terminal.min.css"
                |> AssemblyWide
            ]
            Namespace "SMApp.JQueryTerminal" [
                Terminal
                Options
                EchoOptions
                InterpreterOptions
            ]
        ]


[<Sealed>]
type Extension() =
    interface IExtension with
        member x.Assembly = Definition.Assembly

[<assembly: Extension(typeof<Extension>)>]
do ()
