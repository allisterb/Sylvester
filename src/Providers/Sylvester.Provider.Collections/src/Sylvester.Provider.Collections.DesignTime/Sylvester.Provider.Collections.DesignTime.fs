module Sylvester.Provider.CollectionsImplementation

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
open Sylvester.Collections

[<TypeProvider>]
type CollectionsProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config, assemblyReplacementMap=[("Sylvester.Provider.Collections.DesignTime", "Sylvester.Provider.Collections.Runtime")], addDefaultProbingLocation=true)

    let ns = "Sylvester.Collections"
    let asm = Assembly.GetExecutingAssembly()

    let arrayType  =
        let _type = ProvidedTypeDefinition(asm, ns, "Array", Some typeof<Array<_,_,_,_,_,_,_,_,_,_>>)
        let lengthParam = ProvidedStaticParameter("Length", typeof<int>)
        
        do _type.DefineStaticParameters([lengthParam], fun name args ->
            let _length = args.[0] :?> int
            let p = typedefof<Array<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(_length, 10))
            let provided = ProvidedTypeDefinition(asm, ns, name, Some p, false)
            //provided.GetGenericTypeDefinition().
            //provided.AddXmlDoc <| (sprintf "<summary>A typed array of length %d.</summary>" <| _length)
            
            provided
        )
        //let meth = ProvidedMethod("StaticMethod", [], typeof<DataSource>, isStatic=true, invokeCode = (fun args -> Expr.Value(null, typeof<DataSource>)))
        //varrayType.Def


        //let ctor = ProvidedConstructor([], invokeCode = fun args -> <@@ "My internal state" :> obj @@>)
        //myType.AddMember(ctor)

        //let ctor2 = ProvidedConstructor([ProvidedParameter("InnerState", typeof<string>)], invokeCode = fun args -> <@@ (%%(args.[0]):string) :> obj @@>)
        //myType.AddMember(ctor2)

        //let innerState = ProvidedProperty("InnerState", typeof<string>, getterCode = fun args -> <@@ (%%(args.[0]) :> obj) :?> string @@>)
        //myType.AddMember(innerState)

        //let meth = ProvidedMethod("StaticMethod", [], typeof<DataSource>, isStatic=true, invokeCode = (fun args -> Expr.Value(null, typeof<DataSource>)))
        //myType.AddMember(meth)

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
        _type.AddMember(nameOf)

        _type

    do
        this.AddNamespace(ns, [arrayType])

[<TypeProviderAssembly>]
do ()
