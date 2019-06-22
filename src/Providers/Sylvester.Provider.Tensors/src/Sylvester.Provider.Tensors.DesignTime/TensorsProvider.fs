module Sylvester.Provider.TensorsImplementation

open System
open System.Reflection
open FSharp.Quotations
open FSharp.Core.CompilerServices
open ProviderImplementation
open ProviderImplementation.ProvidedTypes

open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10
open Sylvester.Tensors

[<TypeProvider>]
type TensorsProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config, assemblyReplacementMap=[("Sylvester.Provider.Tensors.DesignTime", "Sylvester.Provider.Tensors.Runtime")], addDefaultProbingLocation=true)

    let ns = "Sylvester.Tensors"
    let asm = Assembly.GetExecutingAssembly()

    let createTypes () =
        let V = ProvidedTypeDefinition(asm, ns, "Vec", Some typeof<Vector<_,_,_,_,_,_,_,_,_,_,_>>, false)   
        let helpText = 
            """<summary>Floating-point vector with type-level dimension constraints.</summary>
           <param name=Length'>The length of the vector.</param>
            """
        V.AddXmlDoc helpText

        let lengthParam = ProvidedStaticParameter("Length", typeof<int>)

        do V.DefineStaticParameters([lengthParam], fun name args ->
            let n = args.[0] :?> int
            let tp = Array.concat [[|typeof<single>|];getIntBase10TypeArray(n, 10)]
            let g = typedefof<Vector<_,_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(tp)
            let provided = ProvidedTypeDefinition(asm, ns, name, Some g, false)
            provided.AddXmlDoc <| (sprintf "<summary>Floating-point vector of length %d with type-level dimension constraints.</summary>" <| n)   
            
            let ctor = ProvidedConstructor([], invokeCode = fun args -> 
                <@@ Activator.CreateInstance(typedefof<Vector<_,_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(Array.concat [[|typeof<single>|];getIntBase10TypeArray(n, 10)])) @@>)
            let ctor2 = ProvidedConstructor([ ProvidedParameter("items",typeof<single[]>) ], invokeCode = fun args -> 
                <@@ Activator.CreateInstance(typedefof<Vector<_,_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(Array.concat [[|typeof<single>|];getIntBase10TypeArray(n, 10)]), (%%(args.[0]) : single[])) @@>)
            let ctor3 = ProvidedConstructor([ ProvidedParameter("value",typeof<single>) ], invokeCode = fun args -> 
                <@@ Activator.CreateInstance(typedefof<Vector<_,_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(Array.concat [[|typeof<single>|];getIntBase10TypeArray(n, 10)]), (%%(args.[0]) : single)) @@>)
  
            provided.AddMember(ctor)
            provided.AddMember(ctor2)
            provided.AddMember(ctor3)
          
            provided
        )

        let M = ProvidedTypeDefinition(asm, ns, "Mat", Some typeof<Matrix<_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_>>, false)   
        let helpText = 
            """<summary>Floating-point matrix with type-level dimension constraints.</summary>
           <param name=Rows'>The number of rows in the matrix.</param>
           <param name=Cols'>The number of columns in the matrix.</param>
            """
        M.AddXmlDoc helpText

        let dim0Param = ProvidedStaticParameter("Rows", typeof<int>)
        let dim1Param = ProvidedStaticParameter("Cols", typeof<int>)

        do M.DefineStaticParameters([dim0Param; dim1Param], fun name args ->
            let d0 = args.[0] :?> int
            let d1 = args.[1] :?> int
            let tp = Array.concat [[|typeof<single>|];getIntBase10TypeArray(d0, 10);getIntBase10TypeArray(d1, 10)]
            let g = typedefof<Matrix<_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(tp)
            let provided = ProvidedTypeDefinition(asm, ns, name, Some g, false)
            provided.AddXmlDoc <| (sprintf "<summary>Floating-point matrix of %d rows and %d columns with type-level dimension constraints.</summary>" d0 d1)   
            
            let ctor = ProvidedConstructor([], invokeCode = fun args -> 
                <@@ Activator.CreateInstance(typedefof<Matrix<_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(Array.concat [[|typeof<single>|];getIntBase10TypeArray(d0, 10); getIntBase10TypeArray(d1, 10)])) @@>)
            let ctor2 = ProvidedConstructor([ ProvidedParameter("items",typeof<single[,]>) ], invokeCode = fun args -> 
                <@@ Activator.CreateInstance(typedefof<Matrix<_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(Array.concat [[|typeof<single>|];getIntBase10TypeArray(d0, 10); getIntBase10TypeArray(d1, 10)]), (%%(args.[0]) : single[,])) @@>)
            let ctor3 = ProvidedConstructor([ ProvidedParameter("value",typeof<single>) ], invokeCode = fun args -> 
                <@@ Activator.CreateInstance(typedefof<Matrix<_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(Array.concat [[|typeof<single>|];getIntBase10TypeArray(d0, 10); getIntBase10TypeArray(d1, 10)]), (%%(args.[0]) : single)) @@>)

            provided.AddMember(ctor)
            provided.AddMember(ctor2)
            provided.AddMember(ctor3)
            provided
        )

        [V; M]

    do
        this.AddNamespace(ns, createTypes())
        
[<TypeProviderAssembly>]
do ()
