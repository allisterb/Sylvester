namespace Sylvester

// Put the TypeProviderAssemblyAttribute in the runtime DLL, pointing to the design-time DLL
[<assembly:CompilerServices.TypeProviderAssembly("Sylvester.Provider.Arithmetic.DesignTime.dll")>]
do ()
