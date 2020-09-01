namespace Sylvester.Web.Thea

open System.Collections.Generic

open WebSharper

[<JavaScript>]
module Main =
    let debug m = ClientExtensions.debug "Main" m
    
    let questions = [ 
        Question("addUser", "Do you want me to add the user $0?")
        Question("switchUser", "Do you want me to switch to the user $0?")
    ]  
    let getQuestion n = questions |> List.tryFind(fun q -> q.Name = n)
    let haveQuestion n = questions |> List.exists(fun q -> q.Name = n)

    let update (cui: CUI) (props: Dictionary<string, obj>) (questions:Stack<Question>) (responses:Stack<string>) (context: Stack<Meaning>) =        
        debug <| sprintf "Begin context: %A." context
        debug <| sprintf "Begin questions: %A." questions
     
        let haveProp k = props.ContainsKey k
        let addProp k v = props.Add(k, v)
        let deleteProp k = props.Remove k |> ignore
        let strProp k = props.[k] :?> string

        let popc() = context.Pop() |> ignore
        let popq() = questions.Pop() |> ignore
        let pushq (n:string) = 
            match getQuestion n with
            | Some q -> questions.Push q
            | None -> failwithf "No such question: %s" n

        let say' t = cui.Say t
        
        let say t =
            responses.Push t
            say' t

        let sayRandom p v  = 
            let t = getRandomPhrase p v
            responses.Push(t) |> ignore
            cui.Say t
        
        let sayRandom' p = sayRandom p ""

        let ask q v =
            addProp q v
            pushq q; 
            debug <| sprintf "Added question: %A." (questions.Peek()) 
            let _q = getQuestion q in say <| replace_tok "$0" v _q.Value.Text
            
        (* Patterns *)
        let (|PropSet|_|) (n:string) :Meaning -> Meaning option =
            function
            | m when haveProp n -> Some m
            | _ -> None

        let (|PropNotSet|_|) (n:string) :Meaning -> Meaning option =
            function
            | m when not (haveProp n) -> Some m
            | _ -> None
         
        let (|Anon|_|) :Meaning -> Meaning option = //(Intent option * Trait option * Entity list option) option =
            function
            | PropNotSet "user" m -> m |> Some 
            | _ -> None

        let (|User|_|) :Meaning -> Meaning option = //(Intent option * Trait option * Entity list option) option =
            function
            | PropSet "user" m -> m |> Some 
            | _ -> None

        let (|Assert|_|) :Meaning -> Meaning option =
            function
            | m when questions.Count = 0 -> 
                popc()
                Some m
            | _ -> None

        let (|Response|_|) (n:string) :Meaning -> (Meaning * obj option) option =
            function
            | m when haveQuestion n && questions.Count > 0  && questions.Peek().Name = n -> 
                popc()
                popq()
                if haveProp n then
                    let d = props.[n]
                    deleteProp n
                    Some(m, Some d)
                else Some(m, None)
            | _ -> None

        let (|Start|_|) :Meaning -> Meaning option=
            function
            | PropNotSet "started" m -> Some m
            | _ -> None

        let (|Str|_|) : obj -> string option =
            function
            | :? string as s -> Some s
            | _ -> None

        (* User functions *)
        let loginUser u = 
            async { 
                do sayRandom waitRetrievePhrases "user name"
                match! Server.GetUser u with 
                | Some u ->
                    do! Server.UpdateUserLastLogin u.Name
                    props.Add("user", u)
                    sayRandom helloUserPhrases <| sprintf "%A" props.["user"]
                    if u.LastLoggedIn.IsSome then say <| sprintf "You last logged in on %A." u.LastLoggedIn.Value
                    
                | None _ -> 
                    say <| sprintf "Sorry I did not find the user name %s." u
                    ask "addUser" u
            } |> Async.Start

        let addUser u = 
            async { 
                do sayRandom waitAddPhrases "user"
                match! Server.AddUser u with 
                | Some _ -> 
                    addProp "user" u
                    say <| sprintf "Hello %A, nice to meet you." props.["user"]
                | None _ -> 
                    say <| sprintf "Sorry I was not able to add the user %s to the system." u
            } |> Async.Start

        (* Interpreter logic begins here *)
        match context |> Seq.take (if context.Count >= 5 then 5 else context.Count) |> List.ofSeq with

        (* Hello *)
        | Anon(Start(Assert(Intent "hello" (None, None))))::[] ->  
                props.Add("started", true)
                sayRandom' helloPhrases
        | Anon(Assert(Intent "hello" (None, None)))::[] -> say "Hello, tell me your user name to get started."

        (* User login *)
        | Anon(Assert(Intent "hello" (None, Some [Entity "contact" u])))::[]  -> loginUser u
            
        (* User add *)
        | Anon(Yes(Response "addUser" (_, Some(Str(user)))))::[] -> addUser user
        | Anon(No(Response "addUser" (_, Some(Str(user)))))::[] -> say <| sprintf "Ok I did not add the user %s." user

        (* User switch *)
        | User(Assert(Intent "hello" (None, Some [Entity "contact" u])))::[] -> 
            async {
                match! Server.GetUser u with
                | Some user -> ask "switchUser" user.Name
                | None -> say <| sprintf "Sorry, the user %s does not exist." u
            } |> Async.Start
        | User(Yes(Response "switchUser" (_, Some(Str(user)))))::[] ->
            props.["user"] <- user
            say <| sprintf "Ok I switched to user %A." user  
        | User(No(Response "switchUser" (_, Some(Str(user)))))::[] -> 
            say <| sprintf "Ok I did not switch to user %s." user
        
        | m -> 
            debug <| sprintf "Did not understand %A." m
            popc()
            say "Sorry I didn't understand what you meant."
            if questions.Count > 0 then 
                let q = Seq.item 0 questions in 
                if haveProp q.Name then 
                    say <| replace_tok "$0" (props.[q.Name] :?> string) q.Text
                else say q.Text

        debug <| sprintf "End context: %A." context
        debug <| sprintf "End questions: %A." questions