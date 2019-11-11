namespace Sylvester.Fabric.Keras

module Attributes = 
    open System

    // Put the TypeProviderAssemblyAttribute in the runtime DLL, pointing to the design-time DLL
    [<assembly:CompilerServices.TypeProviderAssembly("Sylvester.Provider.ND.DesignTime.dll")>]
    do ()
