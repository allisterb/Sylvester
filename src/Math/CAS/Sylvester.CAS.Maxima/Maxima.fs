namespace Sylvester.CAS

open System
open System.Text
open System.Text.RegularExpressions
open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns

open Sylvester
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
            null, exn "Maxima console process did not initialize." |> Failure
    let initialized = 
        match session with
        | Success s -> 
            if s.Expect.Contains("(%i1)", Nullable(2000)).IsMatch then
                s.Send.Line "ratprint:false;"
                s.Send.Line "display2d:false;"
                s.Send.Line "linel:500;"
                s.Send.Line "load(simplify_sum);"
                if s.Expect.Contains("simplify_sum.mac", Nullable(2000)).IsMatch then
                    true
                else
                    err' "Could not set Maxima default options."
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

    member val Input = new StringBuilder()

    
    member val ProcessTimeOut = 2000 with get, set

    member val CurrentInputLine = 1 with get, set
 
module Maxima =
    let private (||||) l r = Microsoft.FSharp.Core.Operators.(|||) l r
    let private outputPattern = """\(%o(\d)+\)\s+(.+)\s+\(%i(\d)+\)\s?"""
    let private outputRegex = new Regex(outputPattern, RegexOptions.Compiled |||| RegexOptions.Multiline)

    let rec sprint (x:Expr) = 
        match x with
        | List list -> "[" + (list |>  List.map sprint |> List.reduce (fun l r -> l + ", " + r)) + "]"
        
        | SpecificCall <@@ (<) @@> (_, _, [l; r]) -> sprintf("(%s) < (%s)") (sprint l) (sprint r)
        | SpecificCall <@@ (<=) @@> (_, _, [l; r]) -> sprintf("(%s) <= (%s)") (sprint l) (sprint r)
        | SpecificCall <@@ (>) @@> (_, _, [l; r]) -> sprintf("(%s) > (%s)") (sprint l) (sprint r)
        | SpecificCall <@@ (>=) @@> (_, _, [l; r]) -> sprintf("(%s) >= (%s)") (sprint l) (sprint r)
        | SpecificCall <@@ (=) @@> (_, _, [l; r]) -> sprintf("(%s) = (%s)") (sprint l) (sprint r)
        
        | SpecificCall <@@ (~-) @@> (_, _, [l]) -> sprintf("-(%s)") (sprint l)
        | SpecificCall <@@ (~+) @@> (_, _, [l]) -> sprintf("+(%s)") (sprint l)
        | SpecificCall <@@ (/) @@> (_, _, [l; r]) -> sprintf("(%s) / (%s)") (sprint l) (sprint r)
        | SpecificCall <@@ (*) @@> (_, _, [l; r]) -> sprintf("(%s) * (%s)") (sprint l) (sprint r)
        | SpecificCall <@@ (+) @@> (_, _, [l; r]) -> sprintf("(%s) + (%s)") (sprint l) (sprint r)
        | SpecificCall <@@ (-) @@> (_, _, [l; r]) -> sprintf("(%s) - (%s)") (sprint l) (sprint r)
        | SpecificCall <@@ ( ** ) @@> (_, _, [l; r]) -> sprintf("%s^(%s)") (sprint l) (sprint r)
        
        | SpecificCall <@@ Microsoft.FSharp.Core.Operators.sin @@> (_, _, [l]) -> sprintf("sin(%s)") (sprint l) 
        | SpecificCall <@@ Microsoft.FSharp.Core.Operators.cos @@> (_, _, [l]) -> sprintf("cos(%s)") (sprint l)
        | SpecificCall <@@ Microsoft.FSharp.Core.Operators.tan @@> (_, _, [l]) -> sprintf("tan(%s)") (sprint l)

        | SpecificCall <@@ Microsoft.FSharp.Core.Operators.log @@> (_, _, [l]) -> sprintf("log(%s)") (sprint l)
        | SpecificCall <@@ Microsoft.FSharp.Core.Operators.min @@> (_, _, [l;r]) -> sprintf("min(%s,%s)") (sprint l) (sprint r)
    
        | PropertyGet(None, Prop "pi", []) -> "%pi"
        | PropertyGet(None, Prop "e", []) -> "%e"

        | Var v -> v.Name
        | ValueWithName(_,_,n) -> n
        | Lambda(x, e) -> sprintf("%A = %s") x (sprint e)
        | _ -> x |> expand |> MathNetExpr.fromQuotation |> MathNet.Symbolics.Infix.format

    let sprintl (exprs: Expr<'t> list) =
        exprs 
        |> List.toArray
        |> Array.skip 1 
        |> Array.fold(fun s e -> sprintf "%s, %s" s (sprint e)) (sprint exprs.[0]) 
        |> sprintf "[%s]"

    let extract_output text =
        let m = outputRegex.Match text 
        if m.Success then 
            ((m.Groups.Item 1).Value, (m.Groups.Item 2).Value, (m.Groups.Item 3).Value) |> Success 
        else 
            sprintf "Could not extract Maxima output from process response: %s" text |> exn |> Failure
        
    let start path = new Maxima(path)
    
    let stop (m:Maxima) = m.ConsoleProcess.Stop()
    
    let send (m:Maxima) (input:string) = 
        m.Input.AppendLine input |> ignore
        !> m.ConsoleSession.Send.Line input 
        >>|> (m.ConsoleSession.Expect.Regex(outputPattern, Nullable(m.ProcessTimeOut))) |> wrap_result'
        >>>= extract_output
        >>= fun (_, r, n) -> 
            do m.CurrentInputLine <- Int32.Parse n
            r

    let mutable defaultInt:Maxima option = None

    let init (s:string) =
        let m = start s
        match m.Initialized with
        | true -> defaultInt <- Some m
        | _ -> failwithf "Failed to initialize the default Maxima interpreter at %s." s

    let send' s = 
        match defaultInt with
        | Some m ->
            match send (defaultInt.Value) s with
            | Success o -> Ok o
            | Failure e -> Error e
        | None -> failwith "The default Maxima interpreter is not initialized."

    let last_output n =
        match defaultInt with
        | Some m -> m.ConsoleSession.LastOutput n
        | None -> failwith "The default Maxima interpreter is not initialzed."

    let last_input n =
        match defaultInt with
        | Some m -> m.ConsoleSession.LastInput n
        | None -> failwith "The default Maxima interpreter is not initialzed."

    let set_stardisp()  =
        match send' "stardisp:true;" with
        | Ok r -> if r.Trim() <> "true" then failwithf "Could not set stardisp variable. Maxima returned %s." r
        | Error e -> failwithf "Could not set stardisp variable. Maxima returned %s." e.Message


