namespace Sylvester.Tensors


module Attributes = 
    open System


    // Put the TypeProviderAssemblyAttribute in the runtime DLL, pointing to the design-time DLL
    [<assembly:CompilerServices.TypeProviderAssembly("Sylvester.Provider.Tensors.DesignTime.dll")>]
    do ()
