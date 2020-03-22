namespace Sylvester

open System.Collections.Concurrent
open System.Linq

open ExpectNet
open Expect

type Maxima(?maximaCmd:string) =
    inherit Runtime()
    let cmd = defaultArg maximaCmd "maxima"
    let p = new ConsoleProcess(cmd, [||])
    let initialized, s = 
        if p.Initialized then 
            let start = tryCatch (p.Start) ()  
            match start with
            | Success _ ->
                true, Expect.Spawn(new ProcessSpawnable(p.Process), System.Environment.NewLine, base.CancellationToken) |> Some
            | Failure exn -> false, None
        else false, None
    let failIfNotInitialized x = if initialized then failwith "The Maxima process is not started." else x

    override x.Initialized = initialized
    
    member x.ConsoleProcess = failIfNotInitialized p  
    
    member x.ConsoleSession = failIfNotInitialized s
   
module Maxima =
    let start path = new Maxima(path)
    let session (m:Maxima) = m.ConsoleSession

    
    