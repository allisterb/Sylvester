module Sylvester.Provider.ArithmeticImplementation

open System
open System.Reflection
open FSharp.Quotations
open FSharp.Core.CompilerServices
open ProviderImplementation
open ProviderImplementation.ProvidedTypes

open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10

[<TypeProvider>]
type ArithmeticProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config, assemblyReplacementMap=[("Sylvester.Provider.Arithmetic.DesignTime", "Sylvester.Provider.Arithmetic.Runtime")], addDefaultProbingLocation=true)

    let ns = "Sylvester.Arithmetic"
    let asm = Assembly.GetExecutingAssembly()

    let createTypes () =
        let N = ProvidedTypeDefinition(asm, ns, "N", Some typeof<N10<_,_,_,_,_,_,_,_,_,_>>, false)
        
        let helpText = 
            """<summary>Typed representation of a natural number.</summary>
           <param name='Val'>The number to represent.</param>
            """
        N.AddXmlDoc helpText

        let valueParam = ProvidedStaticParameter("Val", typeof<int>)

        do N.DefineStaticParameters([valueParam], fun name args ->
            let n = args.[0] :?> int
            let g = typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(n, 10))
            let provided = ProvidedTypeDefinition(asm, ns, name, Some g, false)
            provided.AddXmlDoc <| (sprintf "<summary>A typed representation of the natural number %d.</summary>" <| n)   
            
            let ctor = ProvidedConstructor([], invokeCode = fun args -> 
                <@@ Activator.CreateInstance(typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(n, 10))) @@>)
            provided.AddMember(ctor)

            let p = ProvidedProperty(propertyName = "i", propertyType = g, isStatic = true, getterCode = fun args -> 
                <@@ Activator.CreateInstance(typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(n, 10))) @@>)

            p.AddXmlDocDelayed(fun () -> sprintf "An instance of the typed representation of %d" n)
            provided.AddMember(p)

            provided
        )
        [N]

    do
        this.AddNamespace(ns, createTypes())
        
[<TypeProviderAssembly>]
do ()
