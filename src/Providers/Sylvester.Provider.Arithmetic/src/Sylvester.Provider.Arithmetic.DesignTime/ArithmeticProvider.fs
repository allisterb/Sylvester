module Sylvester.Provider.ArithmeticImplementation

open System
open System.Reflection
open FSharp.Quotations
open FSharp.Core.CompilerServices
open FSharp.Reflection
open ProviderImplementation
open ProviderImplementation.ProvidedTypes

open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10

[<TypeProvider>]
type ArithmeticProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config, assemblyReplacementMap=[("Sylvester.Provider.Arithmetic.DesignTime", "Sylvester.Provider.Arithmetic.Runtime")], addDefaultProbingLocation=true)

    let ns = "Sylvester.Arithmetic"
    let asm = Assembly.GetExecutingAssembly()

    let Nat =
        let N = ProvidedTypeDefinition(asm, ns, "N", Some typeof<N10<_,_,_,_,_,_,_,_,_,_>>, false)
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
        let D = ProvidedTypeDefinition(asm, ns, "dim", Some typeof<obj>, false)
        D.AddXmlDoc "<summary>Typed representation of a set of natural number dimensions.</summary>"
        let valueParams = seq {for i in 2..10 do yield ProvidedStaticParameter(sprintf "Dim%i" i , typeof<int>, 0)}  |> Seq.append [ProvidedStaticParameter(sprintf "Dim%i" 1 , typeof<int>)] |> Seq.toList
        let createDims (name:string) (args:obj[]) =
            let dims = args |> Array.map (fun a -> a :?> int) |> Array.filter(fun a -> a > 0) 
            let dimTypes =  dims |> Array.map (fun a -> typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(a, 10)))           
            let provided = 
                match dims.Length with
                | 1 -> ProvidedTypeDefinition(asm, ns, name, (if dims.[0] = 0 then (Some typeof<Rank<N0>>) else (Some(typedefof<Rank<_>>.MakeGenericType(dimTypes)))), false)
                | 2 -> ProvidedTypeDefinition(asm, ns, name, Some (typedefof<Rank<_,_>>.MakeGenericType(dimTypes)), false)
                | 3 -> ProvidedTypeDefinition(asm, ns, name, Some (typedefof<Rank<_,_,_>>.MakeGenericType(dimTypes)), false)
                | 4 -> ProvidedTypeDefinition(asm, ns, name, Some (typedefof<Rank<_,_,_,_>>.MakeGenericType(dimTypes)), false)
                | 5 -> ProvidedTypeDefinition(asm, ns, name, Some (typedefof<Rank<_,_,_,_,_>>.MakeGenericType(dimTypes)), false)
                | 6 -> ProvidedTypeDefinition(asm, ns, name, Some (typedefof<Rank<_,_,_,_,_,_>>.MakeGenericType(dimTypes)), false)
                | 7 -> ProvidedTypeDefinition(asm, ns, name, Some (typedefof<Rank<_,_,_,_,_,_,_>>.MakeGenericType(dimTypes)), false)
                | 8 -> ProvidedTypeDefinition(asm, ns, name, Some (typedefof<Rank<_,_,_,_,_,_,_,_>>.MakeGenericType(dimTypes)), false)
                | 9 -> ProvidedTypeDefinition(asm, ns, name, Some (typedefof<Rank<_,_,_,_,_,_,_,_,_>>.MakeGenericType(dimTypes)), false)
                | _ -> failwithf "The maximum number of dimensions is 9"
  
            provided.AddXmlDoc (sprintf "<summary>A typed representation of a set of %i natural number dimensions with sizes %s.</summary>" (dimTypes.Length) (Seq.reduce (fun a b -> a + "," + b) (dims |> Array.map (fun d -> d.ToString()))))  
            provided
        D.DefineStaticParameters(valueParams, createDims)
        D
    do
        this.AddNamespace(ns, [Nat; Dim])
        
[<TypeProviderAssembly>]
do ()
