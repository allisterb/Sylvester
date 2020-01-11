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
 
        //G.AddMember(nameOf)
        G

    let Vec =
        let V = ProvidedTypeDefinition(asm, ns, "Vec", Some typeof<Edge>)
        
        let helpText = 
            "<summary>TensorFlow vector with type-level dimension constraints.</summary><param name='Length'>The length or size of the vector.</param>\n<param name='Type'>The TF data type of the vector.</param>"
        V.AddXmlDoc helpText

        let lengthParam = ProvidedStaticParameter("Length", typeof<int>)
        let typeParam = ProvidedStaticParameter("Type", typeof<TF_DataType>)
        
        do V.DefineStaticParameters([lengthParam; typeParam], fun name args ->
            let n = args.[0] :?> int
            let dt = args.[1] :?> TF_DataType
            let N = typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(n, 10))
            let vec = typedefof<Vector<_,_>>.MakeGenericType(N, dt |> dataType)
            //let vecExpr =   vec |> Expr.Value 
            let provided = ProvidedTypeDefinition(asm, ns, name, Some vec, false)
            
            provided.AddXmlDoc <| (sprintf "<summary>%s vector of length %d with type-level dimension constraints.</summary>" (dt.ToString()) (n))   

            let ctor1 = ProvidedConstructor([ProvidedParameter("name",typeof<string>)], invokeCode = fun args -> 
                    <@@ Activator.CreateInstance(typedefof<Vector<_,_>>.MakeGenericType(typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(n, 10)), dt |> dataType), 
                            BindingFlags.NonPublic ||| BindingFlags.Instance, null, [|(%%(args.[0]) : string) :> obj; None :> obj|], null) @@>)
            let ctor2 = ProvidedConstructor([ProvidedParameter("name", typeof<string>); ProvidedParameter("graph", typeof<ITensorGraph>)], invokeCode = fun args -> 
                    <@@ Activator.CreateInstance(typedefof<Vector<_,_>>.MakeGenericType(typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(n, 10)), dt |> dataType), 
                            BindingFlags.NonPublic ||| BindingFlags.Instance, null, [|(%%(args.[0]) : string) :> obj; (Some (%%(args.[1]) : ITensorGraph)) :> obj|], null) @@>)
            
            ctor1.AddXmlDoc(sprintf "<summary>Create a TensorFlow %s vector with the specified name in the default graph.</summary>\n<param name='name'>The name of the vector.</param>" <| dt.ToString())
            ctor2.AddXmlDoc(sprintf "<summary>Create a TensorFlow %s vector with the specified name in the specified graph.</summary>\n<param name='name'>The name of the vector</param>\n<param name='graph'>The graph the vector belongs to.</param>" <| dt.ToString())

            provided.AddMember(ctor1)
            provided.AddMember(ctor2)
           
            provided
        )
        V
 
    let Mat =
        let M = ProvidedTypeDefinition(asm, ns, "Mat", Some typeof<Edge>)
        
        let helpText = 
            "<summary>TensorFlow matrix with type-level dimension constraints.</summary><param name='Dim0'>The number of rows or size of matrix dimension zero.</param>\n<param name='Dim1'>The number of columns or size of matrix dimension one.</param>\n<param name='Type'>The TF data type of the matrix.</param>"
        M.AddXmlDoc helpText

        let dim0Param = ProvidedStaticParameter("Dim0", typeof<int>)
        let dim1Param = ProvidedStaticParameter("Dim1", typeof<int>)
        let typeParam = ProvidedStaticParameter("Type", typeof<TF_DataType>)
        
        do M.DefineStaticParameters([dim0Param; dim1Param; typeParam], fun name args ->
            let dim0 = args.[0] :?> int
            let dim1 = args.[1] :?> int
            let dt = args.[2] :?> TF_DataType
            let N0 = typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(dim0, 10))
            let N1 = typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(dim1, 10))
            let mat = typedefof<Matrix<_,_,_>>.MakeGenericType(N0, N1, dt |> dataType)
            let provided = ProvidedTypeDefinition(asm, ns, name, Some mat, false)
            
            provided.AddXmlDoc <| (sprintf "<summary>%s matrix of dimensions %dx%d with type-level dimension constraints.</summary>" (dt.ToString()) (dim0) (dim1))   

            let ctor1 = ProvidedConstructor([ProvidedParameter("name",typeof<string>)], invokeCode = fun args -> 
                    <@@ 
                        Activator.CreateInstance(typedefof<Matrix<_,_,_>>.MakeGenericType(typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(dim0, 10)), 
                                                    typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(dim1, 10)), dt |> dataType), 
                            BindingFlags.NonPublic ||| BindingFlags.Instance, null, [|(%%(args.[0]) : string) :> obj; None :> obj|], null) 
                    @@>)
            let ctor2 = ProvidedConstructor([ProvidedParameter("name", typeof<string>); ProvidedParameter("graph", typeof<ITensorGraph>)], invokeCode = fun args -> 
                    <@@ 
                        Activator.CreateInstance(typedefof<Matrix<_,_,_>>.MakeGenericType(typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(dim0, 10)), 
                                                    typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(dim1, 10)), dt |> dataType), 
                            BindingFlags.NonPublic ||| BindingFlags.Instance, null, [|(%%(args.[0]) : string) :> obj; (Some (%%(args.[1]) : ITensorGraph)) :> obj|], null) @@>)
            
            ctor1.AddXmlDoc(sprintf "<summary>Create a TensorFlow %s matrix with the specified name in the default graph.</summary>\n<param name='name'>The name of the matrix.</param>" <| dt.ToString())
            ctor2.AddXmlDoc(sprintf "<summary>Create a TensorFlow %s matrix with the specified name in the specified graph.</summary>\n<param name='name'>The name of the matrix.</param>\n<param name='graph'>The graph the matrix belongs to.</param>" <| dt.ToString())
            provided.AddMember(ctor1)
            provided.AddMember(ctor2)

            provided
        )
 
        //M.AddMember(nameOf)
        M

    do this.AddNamespace(ns, [Graph; Vec; Mat])



[<TypeProviderAssembly>]
do ()
