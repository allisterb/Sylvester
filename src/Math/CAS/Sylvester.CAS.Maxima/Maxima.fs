namespace Sylvester

open System
open System.Text
open System.Text.RegularExpressions
open ExpectNet
open Expect

type Maxima(?maximaCmd:string) =
    inherit Runtime()
    let cmd = defaultArg maximaCmd "maxima"
    let p = new ConsoleProcess(cmd, Array.empty, false)
    let output, session = 
        if p.Initialized then
            let _s = new ProcessSpawnable(p.Process, new StringBuilder())
            _s.Output, !> p.Start () >>|> Expect.Spawn(_s, Environment.NewLine, base.CancellationToken)
        else 
            null, exn "Console process did not initialize." |> Failure
    let initialized = 
        match session with
        | Success s -> 
            if s.Expect.Contains("(%i1)", Nullable(2000)).IsMatch then
                s.Send.Line "display2d: false;"
                if s.Expect.Contains("(%o1) false", Nullable(2000)).IsMatch then
                    true
                else
                    err' "Did not set Maxima display2d to false."
                    false
            else
                err' "Did not receive expected response from Maxima console process."
                false
        | Failure f -> 
            err "Could not initialize Maxima." [f]
            false

    let failIfNotInitialized x = if not initialized then failwith "The Maxima process is not started." else x

    override x.Initialized = initialized
    
    member x.ConsoleProcess = failIfNotInitialized p
    
    member x.ConsoleSession = failIfNotInitialized session.Res

    member x.Output = failIfNotInitialized output

    member val ProcessTimeOut = 2000 with get, set

    member val CurrentInputLine = 1 with get, set
 
module Maxima =
    
    let private outputRegex = new Regex("""\(%o(\d)+\)\s+(\S+)\s\(%i(\d)+\)\s+""", RegexOptions.Compiled ||| RegexOptions.Multiline)

    let extract_output text =
        let m = outputRegex.Match text 
        if m.Success then 
            ((m.Groups.Item 1).Value, (m.Groups.Item 2).Value, (m.Groups.Item 3).Value) |> Success 
        else 
            sprintf "Could not extract Maxima output from process response %s." text |> exn |> Failure
        
    let start path = new Maxima(path)
    
    let stop (m:Maxima) = m.ConsoleProcess.Stop()
    
    let send (m:Maxima) (input:string) = 
        !> m.ConsoleSession.Send.Line input 
        >>|> (m.ConsoleSession.Expect.StartsWith("\n(%o", Nullable(m.ProcessTimeOut))) |> wrap_result'
        >>>= extract_output
        >>= fun (_, r, n) -> 
            do m.CurrentInputLine <- Int32.Parse n
            r