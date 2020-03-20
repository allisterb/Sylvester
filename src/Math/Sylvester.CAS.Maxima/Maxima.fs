namespace Sylvester

open ExpectNet

type Maxima(?maximaCmd:string) =
    inherit Runtime()
    let cmd = defaultArg maximaCmd "maxima"
    let p = new ConsoleProcess(cmd, [||])
    do if p.Initialized then p.Start()

    override x.Initialized = p.Initialized && p.IsStarted
    
    member val ConsoleProcess = p 
    
    member val Session = Expect.Spawn(new ProcessSpawnable(p.Process), System.Environment.NewLine, base.CancellationToken)
    
    member x.Send = x.Session.Send.Line

    member x.Expect = x.Session.Expect