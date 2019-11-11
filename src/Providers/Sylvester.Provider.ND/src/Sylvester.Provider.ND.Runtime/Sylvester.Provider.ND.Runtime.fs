namespace Sylvester.Fabric.Keras
open Numpy

type Z(n: int) = inherit NDarray(np.zeros(n))
 

// Put the TypeProviderAssemblyAttribute in the runtime DLL, pointing to the design-time DLL
[<assembly:CompilerServices.TypeProviderAssembly("Sylvester.Provider.ND.DesignTime.dll")>]
do ()
