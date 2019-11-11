namespace Sylvester.Fabric.Keras
open Numpy
open Numpy.Models
type Z(t:Dtype, n: int) = 
    inherit NDarray(np.zeros(n))
    let Z(n: int) = new Z(int32.GetDtype(), n)
 

// Put the TypeProviderAssemblyAttribute in the runtime DLL, pointing to the design-time DLL
[<assembly:CompilerServices.TypeProviderAssembly("Sylvester.Provider.ND.DesignTime.dll")>]
do ()
