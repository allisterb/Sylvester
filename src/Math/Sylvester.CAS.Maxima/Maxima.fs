namespace Sylvester

type Maxima(?maximaCmd:string) =
    let c = defaultArg maximaCmd "maxima"
    let p = new ConsoleProcess(c, Array.empty)
        
       