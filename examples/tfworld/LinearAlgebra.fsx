#r ".\\..\\..\\src\\Providers\\Sylvester.Provider.Arithmetic\\src\\Sylvester.Provider.Arithmetic.Runtime\\bin\\Release\\net45\\Sylvester.Provider.Arithmetic.Runtime.dll"


#r ".\\..\\..\\src\\Providers\\Sylvester.Provider.tf\\src\\Sylvester.Provider.tf.Runtime\\bin\\x64\\Release\\net45\\Sylvester.Api.dll"
#r ".\\..\\..\\src\\Providers\\Sylvester.Provider.tf\\src\\Sylvester.Provider.tf.Runtime\\bin\\x64\\Release\\net45\\Sylvester.Api.Fs.dll"

#r ".\\..\\..\\src\\Providers\\Sylvester.Provider.tf\\src\\Sylvester.Provider.tf.Runtime\\bin\\x64\\Release\\net45\\Sylvester.Collections.dll"
#r ".\\..\\..\\src\\Providers\\Sylvester.Provider.tf\\src\\Sylvester.Provider.tf.Runtime\\bin\\x64\\Release\\net45\\Sylvester.Tensors.dll"
#r ".\\..\\..\\src\\Providers\\Sylvester.Provider.tf\\src\\Sylvester.Provider.tf.Runtime\\bin\\x64\\Release\\net45\\Sylvester.Graphs.dll"
#r ".\\..\\..\\src\\Providers\\Sylvester.Provider.tf\\src\\Sylvester.Provider.tf.Runtime\\bin\\x64\\Release\\net45\\Sylvester.tf.Api.dll"
#r ".\\..\\..\\src\\Providers\\Sylvester.Provider.tf\\src\\Sylvester.Provider.tf.Runtime\\bin\\x64\\Release\\net45\\Sylvester.tf.dll"

#r ".\\..\\..\\src\\Providers\\Sylvester.Provider.tf\\src\\Sylvester.Provider.tf.Runtime\\bin\\x64\\Release\\net45\\Google.ProtoBuf.dll"
#r ".\\..\\..\\src\\Providers\\Sylvester.Provider.tf\\src\\Sylvester.Provider.tf.Runtime\\bin\\x64\\Release\\net45\\Protobuf.Text.dll"

//#r ".\\..\\..\\src\\Providers\\Sylvester.Provider.tf\\src\\Sylvester.Provider.tf.Designtime\\bin\\x64\\Release\\net45\\Sylvester.Provider.tf.Designtime.dll"

#r ".\\..\\..\\src\\Providers\\Sylvester.Provider.tf\\src\\Sylvester.Provider.tf.Runtime\\bin\\x64\\Release\\net45\\Sylvester.Provider.tf.Runtime.dll"
//#r ".\\..\\..\\src\\Providers\\Sylvester.Provider.Tensors\\src\\Sylvester.Provider.Tensors.Runtime\\bin\\Release\\net45\\Mathnet.Numerics.FSharp.dll"
//#r ".\\..\\..\\src\\Providers\\Sylvester.Provider.Arithmetic\\src\\Sylvester.Provider.Arithmetic.Runtime\\bin\\Release\\netstandard2.0\\Sylvester.Provider.Arithmetic.Runtime.dll"
//#r ".\\..\\..\\src\\Providers\\Sylvester.Provider.Tensors\\src\\Sylvester.Provider.Tensors.Runtime\\bin\\Release\\netstandard2.0\\Sylvester.Provider.Tensors.Runtime.dll"
//#r ".\\..\\..\\src\\Providers\\Sylvester.Provider.NDArray\\src\\Sylvester.Provider.NDArray.Runtime\\bin\\Release\\netstandard2.0\\Sylvester.Provider.NDArray.Runtime.dll"


open Sylvester.tf

let x = Vec<12, INT8>("FOO")
