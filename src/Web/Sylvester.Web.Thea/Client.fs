namespace Sylvester.Web.Thea

open System.Collections.Generic

open FSharp.Control

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI
open WebSharper.UI.Client
open WebSharper.UI.Html

open SMApp.JQueryTerminal
open SMApp.WebSpeech
open SMApp.Microphone

[<JavaScript>]
module Client =
   (* CUI state *)
    let mutable CUI = {
        Voice = None
        Mic = None
        Term = Unchecked.defaultof<Terminal>
        Caption = false
    }
    let mutable MicState = MicNotInitialized
    let mutable ClientState = ClientNotInitialzed
    
    (*Console and terminal messages *)
    let echo m = do if not(isNull(CUI.Term)) then CUI.Term.EchoHtml' <| sprintf "%A" m 
    let debug m = debug "CLIENT" m
        
    (* NLU context *)
    let Context = new Stack<Meaning>()
    let Questions = new Stack<Question>()
    let Responses = new Stack<string>()
    let Props = new Dictionary<string, obj>()
    let push (m:Meaning) = Context.Push m; Context

    (* Initialize speech and mic *)
    let synth = Window.SpeechSynthesis
    
    let initSpeech() =
        let voices = synth.GetVoices() |> toArray         
        do voices |> Array.iter(fun v-> 
            if CUI.Voice = None && (v.Name.Contains "Microsoft Zira" || v.Name.ToLower().Contains "female") then
                CUI <- { CUI with Voice = Some v }; debug <| sprintf "Using voice %s." CUI.Voice.Value.Name
            )
        if CUI.Voice = None && voices.Length > 0 then
            let v = voices |> Array.find (fun v -> v.Default) in 
            CUI <- { CUI with Voice = Some v }; debug <| sprintf "Using default voice %s." CUI.Voice.Value.Name 
        else if CUI.Voice = None then 
            echo "No speech synthesis voice is available. Install speech synthesis on this device or computer to use the voice output feature of Selma."
    
    let initMic m (term:Terminal) =
        CUI <- { CUI with Mic = Some(new Mic()) }
        let mic = CUI.Mic.Value
        do mic.onConnecting <- (fun _ -> MicState <- MicConnecting; debug "Mic connecting...")
        do mic.onDisconnected <- (fun _ -> MicState <- MicDisconnected;debug "Mic disconnected.")
        do mic.onAudioStart <- (fun _ -> MicState <- MicAudioStart;debug "Mic audio start...")
        do mic.onAudioEnd <- (fun _ -> MicState <- MicAudioEnd;debug "Mic audio end.")
        do mic.onError <- (fun s -> MicState <- MicError s; debug (sprintf "Mic error : %s." s))
        do mic.onReady <- (fun _ -> MicState <- MicReady; debug "Mic ready.")
        do mic.onResult <- (fun i e -> 
            match ClientState with
            | ClientReady ->
                if not (isNull i || isNull e) then 
                    MicState <- MicResult(i,e) 
                    debug <| sprintf "Mic result: %A %A." i e; m mic (i,e)
                else 
                    debug "Mic: No result returned."
                
            | ClientUnderstand -> echo "I'm still trying to understand what you said before."
            | ClientNotInitialzed -> error "Client is not intialized."
            )
        do mic.Connect("4Y2BLQY5TWLIN7HFIV264S53MY4PCUAT")
    
    let say' text =        
        match CUI.Voice with
        | None -> 
            CUI.Term.Echo' text
        | Some v ->
            async { 
                let u = new SpeechSynthesisUtterance(text)
                u.Voice <- v
                Window.SpeechSynthesis.Speak(u)
                do if CUI.Caption then CUI.Term.Echo' text
            } |> Async.Start

    let say text =
        Responses.Push text
        say' text
        
    let sayVoices() =
        let voices = Window.SpeechSynthesis.GetVoices() |> toArray    
        sprintf "There are currently %i voices installed on this computer or device." voices.Length |> say'
        voices |> Array.iteri (fun i v -> sprintf "Voice %i. Name: %s, Local: %A." i v.Name v.LocalService |> say')

    let sayRandom t phrases = say <| getRandomPhrase phrases t
    
    let wait (f:unit -> unit) =
        do 
            CUI.Term.Echo'("please wait")
            CUI.Term.Pause();f();CUI.Term.Resume()

    let container = SMApp.Bootstrap.Controls.Container

    /// Main interpreter
    let Main =             
        let main' (_:Mic) (command:obj*obj) =
            let i, e = command
            debug <| sprintf "Voice: %A %A" i e
            let intent = 
                match i, e with
                | Voice.Intent' i -> Some i
                | _ -> None
            let _trait = 
                match e with
                | Voice.Trait' t -> Some t
                | _ -> None
            let entity = 
                match e with
                | Voice.Entity' entity -> Some [entity]
                | _ -> None
            match (intent, _trait, entity) with
            | None, None, None -> ()
            | _ -> 
                debug <| sprintf "Voice: %A %A %A" intent _trait entity
                Meaning(intent, _trait, entity) |> push |> Main.update CUI Props Questions Responses
                
        let main (term:Terminal) (command:string)  =
            CUI <- { CUI with Term = term }
            do if CUI.Mic = None then initMic main' term
            do if CUI.Voice = None then initSpeech ()
            do if ClientState = ClientNotInitialzed then ClientState <- ClientReady
            match command with
            (* Quick commands *)
            | Text.Blank -> say' "Tell me what you want me to do or ask me a question."
            | Text.Debug ->  
                debug <| sprintf "Context: %A" Context
                //debug <| sprintf "Properties: %A" Props
                debug <| sprintf "Questions: %A" Questions
            | Text.Voices -> sayVoices()
            | _ ->
                match ClientState with
                | ClientUnderstand -> say' "I'm still trying to understand what you said before."
                | ClientReady ->
                    match command with
                    | Text.QuickHello m 
                    | Text.QuickHelp m 
                    | Text.QuickYes m
                    | Text.QuickNo m
                    | Text.QuickPrograms m -> 
                        debug <| sprintf "Quick Text: %A." m
                        m |> push |> Main.update CUI Props Questions Responses
                        ClientState <- ClientReady
                    (* Use the NLU service for everything else *)
                    | _->         
                        async {
                            ClientState <- ClientUnderstand
                            match! Server.GetMeaning command with
                            | Text.HasMeaning m -> 
                                debug <| sprintf "Text: %A %A %A" m.Intent m.Trait m.Entities
                                m |> push |> Main.update CUI Props Questions Responses
                            | _ -> 
                                debug "Text: Did not receive a meaning from the server." 
                                say' "Sorry I did not understand what you said."
                            ClientState <- ClientReady
                        } |> CUI.Wait
                | ClientNotInitialzed -> error "Client is not initialized."
        let mainOpt =
            Options(
                Name="Main", 
                Greetings = "Welcome to Selma. Enter 'hello my name is...(you)' to begin and initialize speech or help for more assistance.",
                Prompt =">"
            )       
        Interpreter(main', (main, mainOpt))
    
    let run() =
        Terminal("#main", ThisAction<Terminal, string>(fun term command -> Main.Text term command), Main.Options) |> ignore 
        Doc.Empty