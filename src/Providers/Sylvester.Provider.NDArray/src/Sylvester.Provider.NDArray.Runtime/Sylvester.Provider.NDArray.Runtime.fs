namespace Sylvester.Fabrics.Keras

open System

// Put the TypeProviderAssemblyAttribute in the runtime DLL, pointing to the design-time DLL
[<assembly:CompilerServices.TypeProviderAssembly("Sylvester.Provider.NDArray.DesignTime.dll")>]
do ()
