module Sylvester.Provider.ArithmeticImplementation

open System
open System.Reflection
open FSharp.Quotations
open FSharp.Core.CompilerServices
open FSharp.Reflection
open ProviderImplementation
open ProviderImplementation.ProvidedTypes

open Sylvester
open Base10
open Arithmetic

[<TypeProvider>]
type ArithmeticProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config, assemblyReplacementMap=[("Sylvester.Provider.Arithmetic.DesignTime", "Sylvester.Provider.Arithmetic.Runtime")], addDefaultProbingLocation=true)

    let ns = "Sylvester"
    let asm = Assembly.GetExecutingAssembly()

    let Nat =
        let N = ProvidedTypeDefinition(asm, ns, "nat", Some typeof<N10<_,_,_,_,_,_,_,_,_,_>>, false)
        N.AddXmlDoc "<summary>Typed representation of a natural number.</summary>\n<param name='Val'>The number to represent.</param>"
        let valueParam = ProvidedStaticParameter("Val", typeof<int>)
        let createN (name:string) (args:obj[]) =
            let n = args.[0] :?> int
            let g = typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(n, 10))
            let provided = ProvidedTypeDefinition(asm, ns, name, Some g, false)
            provided.AddXmlDoc <| (sprintf "<summary>A typed representation of the natural number %d.</summary>" <| n)   
            
            let ctor = ProvidedConstructor([], invokeCode = fun args -> 
                <@@ Activator.CreateInstance(typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(n, 10))) @@>)
            provided.AddMember(ctor)

            let p = ProvidedMethod("create", [], g, isStatic = true, invokeCode = fun args -> 
                <@@ Activator.CreateInstance(typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(n, 10))) @@>)

            p.AddXmlDocDelayed(fun () -> sprintf "Create an instance of the typed representation of %d." n)
            provided.AddMember(p)
            provided

        N.DefineStaticParameters([valueParam], createN)
        N

    let Dim = 
        let D = ProvidedTypeDefinition(asm, ns, "dim", Some typeof<N10<_,_,_,_,_,_,_,_,_,_>>, false)
        D.AddXmlDoc "<summary>Typed representation of a natural number dimension size.</summary>\n<param name='Val'>The dimension size to represent.</param>"
        let valueParam = ProvidedStaticParameter("Val", typeof<int>)
        let createDim (name:string) (args:obj[]) =
            let n = args.[0] :?> int
            let g = typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(n, 10))
            let provided = ProvidedTypeDefinition(asm, ns, name, Some g, false)
            provided.AddXmlDoc <| (sprintf "<summary>A typed representation of the natural number dimension size %d.</summary>" <| n)   
    
            let ctor = ProvidedConstructor([], invokeCode = fun args -> 
                <@@ Activator.CreateInstance(typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(n, 10))) @@>)
            provided.AddMember(ctor)

            let p = ProvidedMethod("create", [], g, isStatic = true, invokeCode = fun args -> 
                <@@ Activator.CreateInstance(typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(n, 10))) @@>)

            p.AddXmlDocDelayed(fun () -> sprintf "Create an instance of the typed representation of the natural number dimension size %d." n)
            provided.AddMember(p)
            provided

        D.DefineStaticParameters([valueParam], createDim)
        D
    do
        this.AddNamespace(ns, [Nat; Dim])
        
[<TypeProviderAssembly>]
do ()
