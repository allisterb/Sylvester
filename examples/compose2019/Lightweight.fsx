#r ".\\..\\..\src\\Providers\\Sylvester.Provider.Tensors\\src\\Sylvester.Provider.Tensors.Runtime\\bin\\Release\\net45\\Mathnet.Numerics.dll"
#r ".\\..\\..\src\\Providers\\Sylvester.Provider.Tensors\\src\\Sylvester.Provider.Tensors.Runtime\\bin\\Release\\net45\\Mathnet.Numerics.FSharp.dll"
#r ".\\..\\..\\src\\Providers\\Sylvester.Provider.Arithmetic\\src\\Sylvester.Provider.Arithmetic.Runtime\\bin\\Release\\net45\\Sylvester.Provider.Arithmetic.Runtime.dll"
#r ".\\..\\..\\src\\Providers\\Sylvester.Provider.Tensors\\src\\Sylvester.Provider.Tensors.Runtime\\bin\\Release\\net45\\Sylvester.Provider.Tensors.Runtime.dll"


open Sylvester.Arithmetic.Collections
open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10
open Sylvester.Tensors

let vf = new VecF<"C:\\Projects\\Sylvester\\examples\\data\\iris.data", false, 1>()

vf.Dim0

