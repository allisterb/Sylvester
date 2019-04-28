module Sylvester.Provider.ArithmeticImplementation

open System
open System.Collections.Generic
open System.IO
open System.Linq
open System.Reflection
open FSharp.Quotations
open FSharp.Core.CompilerServices
open ProviderImplementation
open ProviderImplementation.ProvidedTypes

open Sylvester

[<TypeProvider>]
type ArithmeticProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config, assemblyReplacementMap=[("Sylvester.Provider.Arithmetic.DesignTime", "Sylvester.Provider.Arithmetic.Runtime")], addDefaultProbingLocation=true)

    let ns = "Sylvester.Arithmetic"
    let asm = Assembly.GetExecutingAssembly()

    // check we contain a copy of runtime files, and are not referencing the runtime DLL
    do assert (typeof<DataSource>.Assembly.GetName().Name = asm.GetName().Name)  

    let createTypes () =
        
        let baseType = typedefof<N5<_,_,_,_,_>>
        
        let N = ProvidedTypeDefinition(asm, ns, "N", Some baseType, true, true, false, false )

        do N.DefineStaticParameters([], fun name args ->
            let n = args.[0] :?> int
            let provided = ProvidedTypeDefinition(asm, ns, name, Some(N.MakeGenericType(getDigits(n))), hideObjectMethods = true)
            provided
        )

        let ctor = ProvidedConstructor([], invokeCode = fun args -> <@@ obj() @@>)
        N.AddMember(ctor)

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
        N.AddMember(nameOf)

        [N]

    do
        this.AddNamespace(ns, createTypes())

[<TypeProviderAssembly>]
do ()
