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
           <param name=Llength'>The length of the vector.</param>
            """
        V.AddXmlDoc helpText

        let lengthParam = ProvidedStaticParameter("Length", typeof<int>)

        do V.DefineStaticParameters([lengthParam], fun name args ->
            let n = args.[0] :?> int
            let tp = Array.concat [[|typeof<float>|];getIntBase10TypeArray(n, 10)]
            let g = typedefof<Vector<_,_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(tp)
            let provided = ProvidedTypeDefinition(asm, ns, name, Some g, false)
            provided.AddXmlDoc <| (sprintf "<summary>Floating-point vector with length %d with type-level dimension constraints.</summary>" <| n)   
            
            let ctor = ProvidedConstructor([], invokeCode = fun args -> 
                <@@ Activator.CreateInstance(typedefof<Vector<_,_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(Array.concat [[|typeof<float>|];getIntBase10TypeArray(n, 10)])) @@>)
            let ctor2 = ProvidedConstructor([ ProvidedParameter("items",typeof<float[]>) ], invokeCode = fun args -> 
                <@@ Activator.CreateInstance(typedefof<Vector<_,_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(Array.concat [[|typeof<float>|];getIntBase10TypeArray(n, 10)]), (%%(args.[0]) : float[])) @@>)

            provided.AddMember(ctor)
            provided.AddMember(ctor2)
            provided
        )
        [V]

    do
        this.AddNamespace(ns, createTypes())
        
[<TypeProviderAssembly>]
do ()
