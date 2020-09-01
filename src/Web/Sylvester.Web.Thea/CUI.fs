namespace Sylvester.Web.Thea

open System

open WebSharper

open SMApp.JQueryTerminal
open SMApp.WebSpeech
open SMApp.Microphone

[<AutoOpen;JavaScript>]
module CUI =
    let rng = Random()
    
    let getRandomPhrase (phrases:List<string>) r = phrases |> List.item(rng.Next(0, phrases.Length)) |> replace_tok "$0" r
    
    let getRandomPhrase' (phrases:List<string>)  = phrases |> List.item(rng.Next(0, phrases.Length)) |> replace_tok "$0" ""

    let helloPhrases = [
        "Welcome!"
        "Welcome, my name is Selma."
        "Welcome to Selma. How can I help?"
        "Hello this is Selma, how can I help?"
        "Hello, I am Selma. How can I help?"
        "Hello, I am Selma. How may I help you now?"
        "I'm Selma. Tell me your name so we can get started."
    ]

    let helloUserPhrases = [
        "Hi $0, welcome back."
        "Welcome $0, nice to see you again."
        "Hello $0."
        "Good to see you $0."
    ]

    let helpPhrases = [
        "What can I help you with $0?"
    ]

    let waitRetrievePhrases = [
        "Ok, let me check that $0 for you"
        "Please wait while I check that $0 for you."
        "Wait while I check that $0."
        "Ok let me see if I can find that $0."
    ]

    let waitAddPhrases = [
        "Ok, let me add that $0 for you"
        "Please wait while I add that $0 for you."
        "Wait while I add that $0."
    ]

    type User = {
        Name:string
        LastLoggedIn:string option
    }
    with 
        override x.ToString() = x.Name
        
    type MicState = MicNotInitialized | MicConnecting | MicDisconnected | MicAudioStart | MicAudioEnd | MicReady | MicError of string | MicResult of obj * obj

    type ClientState = ClientNotInitialzed | ClientReady | ClientUnderstand 

    type Interpreter = Interpreter of (SMApp.Microphone.Mic -> (obj*obj) -> unit) * ((SMApp.JQueryTerminal.Terminal->string->unit) * SMApp.JQueryTerminal.Options)
        with
        member x.Unwrap = match x with | Interpreter(v, (i, o)) -> v, i, o
        member x.Voice = let v, i, o = x.Unwrap in v
        member x.Text = let v, i, o = x.Unwrap in i
        member x.Options = let v, i, o = x.Unwrap in o

    type CUI = {
         Voice:SpeechSynthesisVoice option
         Mic: Mic option
         Term: Terminal
         Caption: bool
     }
     with
         member x.Echo' (text:string) = x.Term.Disable(); x.Term.Echo text; x.Term.Enable()
 
         member x.EchoHtml' (text:string) = 
             let rawOpt = EchoOptions(Raw=true)
             x.Term.Disable(); x.Term.Echo(text, rawOpt); x.Term.Enable()
 
         member x.Debug loc m = debug loc m
 
         member x.Say text = 
             match x.Voice with
             | None -> x.Echo' text
             | Some v ->
                 async { 
                     let u = new SpeechSynthesisUtterance(text)
                     u.Voice <- v
                     Window.SpeechSynthesis.Speak(u) 
                 } |> Async.Start
                 do if x.Caption then x.Echo' text
 
         member x.sayRandom phrases t = x.Say <| getRandomPhrase phrases t
     
         member x.Wait (f:unit -> unit) =
             do 
                 x.Echo'("please wait...")
                 x.Term.Pause();f();x.Term.Resume()
 
         member x.Wait(f:Async<unit>) = x.Wait(fun _ -> f |> Async.Start)
 
         member x.SayVoices() =
             let voices' = Window.SpeechSynthesis.GetVoices()
             do if not(isNull(voices')) then
                 let voices = voices' |> toArray    
                 sprintf "There are currently %i voices installed on this computer or device." voices.Length |> x.Say
                 voices |> Array.iteri (fun i v -> sprintf "Voice %i. Name: %s, Local: %A." i v.Name v.LocalService |> x.Say)