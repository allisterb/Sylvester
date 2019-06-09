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
           <param name='Value'>The number to represent.</param>
            """
        N.AddXmlDoc helpText

        let valueParam = ProvidedStaticParameter("Value", typeof<int>)

        do N.DefineStaticParameters([valueParam], fun name args ->
            let n = args.[0] :?> int
            let g = typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntDigitType(54321))
            let provided = ProvidedTypeDefinition(asm, ns, name, Some g, false)
            provided.AddXmlDoc <| (sprintf "<summary>A typed representation of the natural number %d.</summary>" <| n)   
            let ctor = ProvidedConstructor([], invokeCode = fun args -> 
                <@@ Activator.CreateInstance(typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntDigitType(54321))) @@>)
            provided.AddMember(ctor)
            provided
        )
        [N]

    do
        this.AddNamespace(ns, createTypes())
        
[<TypeProviderAssembly>]
do ()
