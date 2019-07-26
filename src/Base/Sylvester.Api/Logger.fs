namespace Sylvester

open System

/// Logging interface
[<AbstractClass>]
type Logger() =
    static member val IsConfigured = false with get,set
    
    abstract member Info : messageTemplate:string * [<ParamArray>]args:obj[] -> unit
    abstract member Debug : messageTemplate:string * [<ParamArray>]args:obj[] -> unit
    abstract member Error : messageTemplate:string * [<ParamArray>]args:obj[] -> unit
    abstract member Error : ex:Exception * messageTemplate:string * [<ParamArray>]args:obj[] -> unit
    abstract member Begin : messageTemplate:string * [<ParamArray>]args:obj[] -> IDisposable


/// Basic console logger available anywhere
type ConsoleLogger() =
    inherit Logger()

    override __.Debug (messageTemplate:string, [<ParamArray>] args: obj[]) = 
        String.Format(messageTemplate, args) |> printfn "%s"
    override __.Info (messageTemplate:string, [<ParamArray>] args: obj[])  = 
        String.Format(messageTemplate, args) |> printfn "%s"
    override __.Error (messageTemplate:string, [<ParamArray>] args: obj[])  = 
        String.Format(messageTemplate, args) |> printfn "%s"
    override __.Error (ex: Exception, messageTemplate:string, [<ParamArray>] args: obj[])  = 
        String.Format(messageTemplate, args) |> printfn "%s"

    override __.Begin (messageTemplate:string, [<ParamArray>] args: obj[]) = new ConsoleLoggerOperation(messageTemplate, args) :> IDisposable

and ConsoleLoggerOperation(messageTemplate:string, [<ParamArray>] args: obj[]) = 
    let message = String.Format(messageTemplate, args)
    let start = DateTime.Now
    do printfn "Begin %s..." message
    
    interface IDisposable with
        member __.Dispose() = let completed = DateTime.Now.Subtract(start) in do printfn "Completed %s in %.0f ms." message (completed.TotalMilliseconds)

