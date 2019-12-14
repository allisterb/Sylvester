module Sylvester.Provider.NDImplementation

open System
open System.Collections.Generic
open System.IO
open System.Linq
open System.Reflection

open FSharp.Quotations
open FSharp.Core.CompilerServices
open ProviderImplementation
open ProviderImplementation.ProvidedTypes

open Numpy
open Numpy.Models
open Python.Runtime

open Sylvester.Arithmetic
open Sylvester.Arithmetic.Base10
open Sylvester.Arithmetic.N10

[<TypeProvider>]
type NDArrayProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config, assemblyReplacementMap=[("Sylvester.Provider.ND.DesignTime", "Sylvester.Provider.ND.Runtime")], addDefaultProbingLocation=true)

    let ns = "Sylvester.Fabric.Keras"
    let asm = Assembly.GetExecutingAssembly()
    
    let createTypes () =
        let ND = ProvidedTypeDefinition(asm, ns, "ND", Some typeof<Sylvester.Fabric.Keras.Z>)
        let helpText = 
            """<summary>N-dimensional tensor with type-level dimension constraints for use with the Keras fabric.</summary>
           <param name=dim0'>Length of the first dimension.</param>
           <param name=dim1'>Length of the second dimension.</param>
           <param name=dim2'>Length of the third dimension.</param>
           <param name=dim3'>Length of the fourth dimension.</param>
           <param name=dim4'>Length of the fifth dimension.</param>
           <param name=dim5'>Length of the sixth dimension.</param>
           <param name=dim6'>Length of the seventh dimension.</param>
           <param name=dim7'>Length of the eight dimension.</param>
           <param name=dim8'>Length of the nineth dimension.</param>
           <param name=dim9'>Length of the tenth dimension.</param>
           <param name=dim10'>Length of the eleventh dimension.</param>
           <param name=dim11'>Length of the twelfth dimension.</param>
           <param name=dim12'>Length of the thirteenth dimension.</param>
           <param name=dim13'>Length of the fourteenth dimension.</param>
           <param name=dim14'>Length of the fifteenth dimension.</param>
            """
        let dp0 = ProvidedStaticParameter("dim0", typeof<int>)
        let dp1 = ProvidedStaticParameter("dim1", typeof<int>, 0)
        let dp2 = ProvidedStaticParameter("dim2", typeof<int>, 0)
        let dp3 = ProvidedStaticParameter("dim3", typeof<int>, 0)
        let dp4 = ProvidedStaticParameter("dim4", typeof<int>, 0)
        let dp5 = ProvidedStaticParameter("dim5", typeof<int>, 0)
        let dp6 = ProvidedStaticParameter("dim6", typeof<int>, 0)
        let dp7 = ProvidedStaticParameter("dim7", typeof<int>, 0)
        let dp8 = ProvidedStaticParameter("dim8", typeof<int>, 0)
        let dp9 = ProvidedStaticParameter("dim9", typeof<int>, 0)
        let dp10 = ProvidedStaticParameter("dim10", typeof<int>, 0)
        let dp11 = ProvidedStaticParameter("dim11", typeof<int>, 0)
        let dp12 = ProvidedStaticParameter("dim12", typeof<int>, 0)
        let dp13 = ProvidedStaticParameter("dim13", typeof<int>, 0)
        let dp14 = ProvidedStaticParameter("dim14", typeof<int>, 0)
        
        do ND.DefineStaticParameters([dp0; dp1; dp2; dp3; dp4; dp5; dp6; dp7; dp8; dp9; dp10; dp11; dp12; dp13; dp14], fun name args ->
            let _d0 = args.[0] :?> int
            let _d1 = args.[1] :?> int
            let _d2 = args.[2] :?> int
            let _d3 = args.[3] :?> int
            let _d4 = args.[4] :?> int
            let _d5 = args.[5] :?> int
            let _d6 = args.[6] :?> int
            let _d7 = args.[7] :?> int
            let _d8 = args.[8] :?> int
            let _d9 = args.[9] :?> int
            let _d10 = args.[10] :?> int
            let _d11 = args.[11] :?> int
            let _d12 = args.[12] :?> int
            let _d13 = args.[13] :?> int
            let _d14 = args.[14] :?> int
                        
            let d0 = typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(_d0, 10))
            let d1 = typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(_d1, 10))
            let d2 = typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(_d2, 10))
            let d3 = typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(_d3, 10))
            let d4 = typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(_d4, 10))
            let d5 = typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(_d5, 10))
            let d6 = typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(_d6, 10))
            let d7 = typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(_d7, 10))
            let d8 = typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(_d8, 10))
            let d9 = typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(_d9, 10))
            let d10 = typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(_d10, 10))
            let d11 = typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(_d11, 10))
            let d12 = typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(_d12, 10))
            let d13 = typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(_d13, 10))
            let d14 = typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(_d14, 10))
            
            let dim0 = ProvidedProperty(propertyName = "dim0", propertyType = d0, isStatic = false, getterCode = fun args -> 
                <@@ Activator.CreateInstance(typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(_d0, 10))) @@>)
            let dim1 = ProvidedProperty(propertyName = "dim1", propertyType = d1, isStatic = false, getterCode = fun args -> 
                <@@ Activator.CreateInstance(typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(_d1, 10))) @@>)
            let dim2 = ProvidedProperty(propertyName = "dim2", propertyType = d2, isStatic = false, getterCode = fun args -> 
                <@@ Activator.CreateInstance(typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(_d2, 10))) @@>)
            let dim3 = ProvidedProperty(propertyName = "dim3", propertyType = d3, isStatic = false, getterCode = fun args -> 
                <@@ Activator.CreateInstance(typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(_d3, 10))) @@>)
            let dim4 = ProvidedProperty(propertyName = "dim4", propertyType = d4, isStatic = false, getterCode = fun args -> 
                <@@ Activator.CreateInstance(typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(_d4, 10))) @@>)
            let dim5 = ProvidedProperty(propertyName = "dim5", propertyType = d5, isStatic = false, getterCode = fun args -> 
                <@@ Activator.CreateInstance(typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(_d5, 10))) @@>)
            let dim6 = ProvidedProperty(propertyName = "dim6", propertyType = d6, isStatic = false, getterCode = fun args -> 
                <@@ Activator.CreateInstance(typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(_d6, 10))) @@>)
            let dim7 = ProvidedProperty(propertyName = "dim7", propertyType = d7, isStatic = false, getterCode = fun args -> 
                <@@ Activator.CreateInstance(typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(_d7, 10))) @@>)
            let dim8 = ProvidedProperty(propertyName = "dim8", propertyType = d8, isStatic = false, getterCode = fun args -> 
                <@@ Activator.CreateInstance(typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(_d8, 10))) @@>)
            let dim9 = ProvidedProperty(propertyName = "dim9", propertyType = d9, isStatic = false, getterCode = fun args -> 
                <@@ Activator.CreateInstance(typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(_d9, 10))) @@>)
            let dim10 = ProvidedProperty(propertyName = "dim10", propertyType = d10, isStatic = false, getterCode = fun args -> 
                <@@ Activator.CreateInstance(typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(_d10, 10))) @@>)
            let dim11 = ProvidedProperty(propertyName = "dim11", propertyType = d11, isStatic = false, getterCode = fun args -> 
                <@@ Activator.CreateInstance(typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(_d11, 10))) @@>)
            let dim12 = ProvidedProperty(propertyName = "dim12", propertyType = d12, isStatic = false, getterCode = fun args -> 
                <@@ Activator.CreateInstance(typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(_d12, 10))) @@>)
            let dim13 = ProvidedProperty(propertyName = "dim13", propertyType = d13, isStatic = false, getterCode = fun args -> 
                <@@ Activator.CreateInstance(typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(_d13, 10))) @@>)
            let dim14 = ProvidedProperty(propertyName = "dim14", propertyType = d14, isStatic = false, getterCode = fun args -> 
                <@@ Activator.CreateInstance(typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(_d14, 10))) @@>)

            let ctor0 = ProvidedConstructor([], invokeCode = fun args -> <@@ new Sylvester.Fabric.Keras.Z(float32.GetDtype(), _d0) @@>)

            let ctor = ProvidedConstructor([ProvidedParameter("dtype", typeof<Dtype>)], invokeCode = fun args -> <@@ new Sylvester.Fabric.Keras.Z((%%(args.[0]) : Dtype), 5) @@>)

            let provided = ProvidedTypeDefinition(asm, ns, name, Some typeof<Sylvester.Fabric.Keras.Z>, false)

            provided.AddMember(ctor0)
            provided.AddMember(ctor)
            provided.AddMember(dim0)
            provided.AddMember(dim1)
            provided.AddMember(dim2)
            provided.AddMember(dim3)
            provided.AddMember(dim4)
            provided.AddMember(dim5)
            provided.AddMember(dim6)
            provided.AddMember(dim7)
            provided.AddMember(dim8)
            provided.AddMember(dim9)
            provided.AddMember(dim10)
            provided.AddMember(dim11)
            provided.AddMember(dim12)
            provided.AddMember(dim13)
            provided.AddMember(dim14)

            let nameOf =
                let param = ProvidedParameter("p", typeof<Microsoft.FSharp.Quotations.Expr<int>>)
                param.AddCustomAttribute {
                    new CustomAttributeData() with
                        member __.Constructor = typeof<ReflectedDefinitionAttribute>.GetConstructor([||])
                        member __.ConstructorArguments = [||] :> _
                        member __.NamedArguments = [||] :> _
                }
                ProvidedMethod("NameOf", [ param ], typeof<string>, isStatic = true, invokeCode = fun args ->
                    <@@
                        match (%%args.[0]) : Microsoft.FSharp.Quotations.Expr<int> with
                        | Microsoft.FSharp.Quotations.Patterns.ValueWithName (_, _, n) -> "tests"
                        | e -> failwithf "Invalid quotation argument (expected ValueWithName): %A" e
                    @@>)
            provided.AddMember(nameOf)
            provided
            // let meth = ProvidedMethod("StaticMethod", [], typeof<DataSource>, isStatic=true, invokeCode = (fun args -> Expr.Value(null, typeof<DataSource>)))
            // myType.AddMember(meth)
        )
        [ND]

    do
        this.AddNamespace(ns, createTypes())

[<TypeProviderAssembly>]
do ()
