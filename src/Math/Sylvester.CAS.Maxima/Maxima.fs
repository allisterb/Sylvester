namespace Sylvester

open System.Collections.Concurrent
open System.Linq

open ExpectNet

type Maxima(?maximaCmd:string) =
    inherit Runtime()
    let cmd = defaultArg maximaCmd "maxima"
    let p = new ConsoleProcess(cmd, Array.empty, false)
    let output, session = 
        if p.Initialized then
            let _s = new ProcessSpawnable(p.Process, new System.Text.StringBuilder())
            _s.Output, !> (p.Start) () >>|= Expect.Spawn(_s, System.Environment.NewLine, base.CancellationToken)
        else 
            null, exn "Console process did not initialize." |> Failure
    let initialized = 
        match session with
        | Success s -> 
            if s.Expect.Contains("(%i1)", new System.Nullable<int>(2000)).IsMatch then
                true
            else
                err' "Did not receive expected response from Maxima process."
                false
        | Failure f -> 
            err "Could not initialize Maxima." [f]
            false

    let failIfNotInitialized x = if not initialized then failwith "The Maxima process is not started." else x

    override x.Initialized = initialized
    
    member x.ConsoleProcess = failIfNotInitialized p
    
    member x.ConsoleSession = failIfNotInitialized session.Res

    member x.Output = failIfNotInitialized output
   
module Maxima =
    
    let start path = new Maxima(path)
    
    let console_process (m:Maxima) = m.ConsoleProcess 
    
    let session (m:Maxima) = m.ConsoleSession
    
    let send (m:Maxima) (input:string) = 
        do m.ConsoleSession.Send.Line input
        let e = m.ConsoleSession.Expect.StartsWith("\n(%o") 
        match e.IsMatch with
        | true -> e.Text |> Success
        | false -> e.Text |> Failure