module Sylvester.Provider.tfImplementation

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open FSharp.Quotations
open FSharp.Core.CompilerServices

open ProviderImplementation
open ProviderImplementation.ProvidedTypes

open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10
open Sylvester.tf
open TensorFlow

[<TypeProvider>]
type SyntaxProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config, assemblyReplacementMap=[("Sylvester.Provider.tf.DesignTime", "Sylvester.Provider.tf.Runtime")], addDefaultProbingLocation=true)

    let ns = "Sylvester.tf"
    let asm = Assembly.GetExecutingAssembly()
    
    let nameOf =
        let param = ProvidedParameter("p", typeof<Expr<int>>)
        param.AddCustomAttribute {
            new CustomAttributeData() with
                member __.Constructor = typeof<ReflectedDefinitionAttribute>.GetConstructor([||])
                member __.ConstructorArguments = [||] :> _
                member __.NamedArguments = [||] :> _
        }
        ProvidedMethod("NameOf", [ param ], typeof<string>, isStatic = true, invokeCode = fun args ->
            <@@
                match (%%args.[0]) : Expr<int> with
                | Microsoft.FSharp.Quotations.Patterns.ValueWithName (_, _, n) -> n
                | e -> failwithf "Invalid quotation argument (expected ValueWithName): %A" e
            @@>)
       
    let Dimension =
        let D = ProvidedTypeDefinition(asm, ns, "d", Some typeof<N10<_,_,_,_,_,_,_,_,_,_>>, false)
    
        let helpText = "<summary>Typed representation of a natural number dimension size.</summary>\n
           <param name='Val'>The dimension size to represent.</param>"
        
        D.AddXmlDoc helpText
    
        let valueParam = ProvidedStaticParameter("Val", typeof<int>)

        do D.DefineStaticParameters([valueParam], fun name args ->
            let d = args.[0] :?> int
            let provided = ProvidedTypeDefinition(asm, ns, name, Some <| typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(d, 10)), false)
            provided.AddXmlDoc <| (sprintf "<summary>A typed representation of the natural number dimension size %i.</summary>" <| d)   
            
            let ctor = ProvidedConstructor([], invokeCode = fun args -> 
                <@@ Activator.CreateInstance(typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(d, 10))) @@>)
            ctor.AddXmlDoc(sprintf "Create an instance of the typed representation of dimension size %i." d)
            provided.AddMember(ctor)

            let create = ProvidedMethod("create", [], typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(d, 10)), isStatic = true, invokeCode = fun args -> 
                <@@ Activator.CreateInstance(typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(d, 10))) @@>)

            create.AddXmlDocDelayed(fun () -> sprintf "Create an instance of the typed representation of dimension %i." d)
            provided.AddMember(create)

            provided
        )
        D

    let Graph =
        let G = ProvidedTypeDefinition(asm, ns, "Graph", Some typeof<TensorGraph<_,_>>)
        
        let helpText = 
            "<summary>TensorFlow graph with type-level input and output constraints.</summary><param name='input'>The number of graph inputs.</param>\n<param name='output'>The number of graph outputs.</param>"
        G.AddXmlDoc helpText

        let inputParam = ProvidedStaticParameter("input", typeof<int>)
        let outputParam = ProvidedStaticParameter("output", typeof<int>)

        do G.DefineStaticParameters([inputParam; outputParam], fun name args ->
            let input = args.[0] :?> int
            let tinput = typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(input, 10))
            let output = args.[1] :?> int
            let toutput = typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(output, 10))
            let g = typedefof<TensorGraph<_,_>>.MakeGenericType(tinput, toutput)
            let provided = ProvidedTypeDefinition(asm, ns, name, Some g, false)
            
            provided.AddXmlDoc <| (sprintf "<summary>TensorFlow graph with %i input(s) and %i output(s).</summary>" input output)   
            
            let ctor1 = ProvidedConstructor([], invokeCode = fun args -> 
                    <@@ 
                    Activator.CreateInstance(typedefof<TensorGraph<_,_>>.MakeGenericType(typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(input, 10)), 
                                                typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(output, 10))), None) 
                    @@>)
            let ctor2 = ProvidedConstructor([ProvidedParameter("scope", typeof<string>)], invokeCode = fun args -> 
                <@@ 
                Activator.CreateInstance(typedefof<TensorGraph<_,_>>.MakeGenericType(typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(input, 10)), 
                                                typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(output, 10))), Some (%%(args.[0]) : string)) 
                @@>)

            ctor1.AddXmlDoc(sprintf "<summary>Create a TensorFlow graph with %i input(s) and %i output(s) with blank root namescope.</summary>" (input) (output))
            ctor2.AddXmlDoc(sprintf "<summary>Create a TensorFlow graph with %i inputs and %i outputs with the specified root namescope.</summary>\n<param name='scope'>The root namescope of the graph.</param>" (input) (output))
            provided.AddMember(ctor1)
            provided.AddMember(ctor2)

            provided
        )
        G

       
    do this.AddNamespace(ns, [Dimension;Graph])

[<TypeProviderAssembly>]
do ()
