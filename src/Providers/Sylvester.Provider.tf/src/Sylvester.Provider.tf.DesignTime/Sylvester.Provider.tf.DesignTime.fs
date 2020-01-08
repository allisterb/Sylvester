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

    let Vec() =
        let V = ProvidedTypeDefinition(asm, ns, "Vec", Some typedefof<Vector<_,_>>)
        
        let helpText = 
            """<summary>Vector with type-level dimension constraints.</summary>
           <param name='Length'>The length of the vector.</param>
           <param name='Type'>The datatype of the vector.</param>
            """
        V.AddXmlDoc helpText

        let lengthParam = ProvidedStaticParameter("Length", typeof<int>)
        let typeParam = ProvidedStaticParameter("Type", typeof<int>)

        do V.DefineStaticParameters([lengthParam; typeParam], fun name args ->
            let n = args.[0] :?> int
            let dt = args.[1] :?> int
            let N = typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(n, 10))
            let vec = typedefof<Vector<_,_>>.MakeGenericType(N, dt |> dataType)
            let vecExpr = Expr.Value vec
            let provided = ProvidedTypeDefinition(asm, ns, name, Some vec, false)
            provided.AddXmlDoc <| (sprintf "<summary>%s vector of length %d with type-level dimension constraints.</summary>" (enum<TF_DataType>(dt).ToString()) (n))   
    
            let ctor1 = ProvidedConstructor([ ProvidedParameter("name",typeof<string>) ], invokeCode = fun args -> 
                <@@ Activator.CreateInstance((%%(vecExpr) : Type), (%%(args.[0]) : string)) @@>)
            
            provided.AddMember(ctor1)
           
            provided
        )
 
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
       
        V.AddMember(nameOf)
        V

    do this.AddNamespace(ns, [Vec()])



[<TypeProviderAssembly>]
do ()
