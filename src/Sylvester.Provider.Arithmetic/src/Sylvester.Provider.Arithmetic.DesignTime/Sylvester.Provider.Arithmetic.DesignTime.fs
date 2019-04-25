module Sylvester.Provider.ArithmeticImplementation

open System
open System.Collections.Generic
open System.IO
open System.Linq
open System.Reflection
open FSharp.Quotations
open FSharp.Core.CompilerServices
open MyNamespace
open ProviderImplementation
open ProviderImplementation.ProvidedTypes

open Sylvester.Base10Digits
open Sylvester.Base10N16


[<AutoOpen>]
module internal TypeHelpers =

    let getDigit(n:int) =
        match n with
        | 0 -> typedefof<_0>
        | 1 -> typedefof<_1>
        | 2 -> typedefof<_2>
        | 3 -> typedefof<_3>
        | 4 -> typedefof<_4>
        | 5 -> typedefof<_5>
        | 6 -> typedefof<_6>
        | 7 -> typedefof<_7>
        | 8 -> typedefof<_8>
        | _ -> failwith "Invalid digit."

    let getDigits (d:int) =
        [| for i in d.ToString() do yield Int32.Parse(i.ToString()) |> getDigit |] //Quick and dirty way to extract digits from number

[<TypeProvider>]
type ArithmeticProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config, assemblyReplacementMap=[("Sylvester.Provider.Arithmetic.DesignTime", "Sylvester.Provider.Arithmetic.Runtime")], addDefaultProbingLocation=true)

    let ns = "Sylvester.Arithmetic"
    let asm = Assembly.GetExecutingAssembly()

    // check we contain a copy of runtime files, and are not referencing the runtime DLL
    do assert (typeof<DataSource>.Assembly.GetName().Name = asm.GetName().Name)  

    let createTypes () =
        
        let baseType = typedefof<N16b<_,_,_,_,_>>
        
        let N = ProvidedTypeDefinition(asm, ns, "N", Some baseType)

        
        let ctor = ProvidedConstructor([], invokeCode = fun args -> <@@ "My internal state" :> obj @@>)
        N.AddMember(ctor)

        let ctor2 = ProvidedConstructor([ProvidedParameter("InnerState", typeof<string>)], invokeCode = fun args -> <@@ (%%(args.[0]):string) :> obj @@>)
        N.AddMember(ctor2)

        let innerState = ProvidedProperty("InnerState", typeof<string>, getterCode = fun args -> <@@ (%%(args.[0]) :> obj) :?> string @@>)
        N.AddMember(innerState)

        let meth = ProvidedMethod("StaticMethod", [], typeof<DataSource>, isStatic=true, invokeCode = (fun args -> Expr.Value(null, typeof<DataSource>)))
        N.AddMember(meth)

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
