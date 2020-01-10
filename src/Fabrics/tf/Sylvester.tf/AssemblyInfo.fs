module AssemblyInfo

open System.Runtime.CompilerServices

[<assembly: InternalsVisibleTo("Sylvester.Tests.tf")>]
[<assembly: InternalsVisibleTo("Sylvester.Provider.tf.DesignTime")>]
[<assembly: InternalsVisibleTo("Sylvester.Provider.tf.Runtime")>]
do()