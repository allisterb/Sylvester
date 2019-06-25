module Sylvester.Provider.TensorsImplementation

open System
open System.Reflection
open FSharp.Quotations
open FSharp.Core.CompilerServices
open ProviderImplementation
open ProviderImplementation.ProvidedTypes

open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10
open Sylvester.Tensors
open Sylvester.Tensors.IO

[<TypeProvider>]
type TensorsProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config, assemblyReplacementMap=[("Sylvester.Provider.Tensors.DesignTime", "Sylvester.Provider.Tensors.Runtime")], addDefaultProbingLocation=true)

    let ns = "Sylvester.Tensors"
    let asm = Assembly.GetExecutingAssembly()

    let createTypes () =
        let V = ProvidedTypeDefinition(asm, ns, "Vec", Some typeof<Vector<_,_,_,_,_,_,_,_,_,_,_>>, false)   
        let helpText = 
            """<summary>Floating-point vector with type-level dimension constraints.</summary>
           <param name=Length'>The length of the vector.</param>
            """
        V.AddXmlDoc helpText

        let lengthParam = ProvidedStaticParameter("Length", typeof<int>)

        do V.DefineStaticParameters([lengthParam], fun name args ->
            let n = args.[0] :?> int
            let tp = Array.concat [[|typeof<single>|];getIntBase10TypeArray(n, 10)]
            let g = typedefof<Vector<_,_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(tp)
            let provided = ProvidedTypeDefinition(asm, ns, name, Some g, false)
            provided.AddXmlDoc <| (sprintf "<summary>Floating-point vector of length %d with type-level dimension constraints.</summary>" <| n)   
            
            let ctor = ProvidedConstructor([], invokeCode = fun args -> 
                <@@ Activator.CreateInstance(typedefof<Vector<_,_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(Array.concat [[|typeof<single>|];getIntBase10TypeArray(n, 10)])) @@>)
            let ctor2 = ProvidedConstructor([ ProvidedParameter("items",typeof<single[]>) ], invokeCode = fun args -> 
                <@@ Activator.CreateInstance(typedefof<Vector<_,_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(Array.concat [[|typeof<single>|];getIntBase10TypeArray(n, 10)]), (%%(args.[0]) : single[])) @@>)
            let ctor3 = ProvidedConstructor([ ProvidedParameter("value",typeof<single>) ], invokeCode = fun args -> 
                <@@ Activator.CreateInstance(typedefof<Vector<_,_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(Array.concat [[|typeof<single>|];getIntBase10TypeArray(n, 10)]), (%%(args.[0]) : single)) @@>)
  
            provided.AddMember(ctor)
            provided.AddMember(ctor2)
            provided.AddMember(ctor3)
          
            provided
        )

        let VF = ProvidedTypeDefinition(asm, ns, "VecF", Some typeof<Vector<_,_,_,_,_,_,_,_,_,_,_>>, false)   
        let helpText = 
            """<summary>Floating-point vector with type-level dimension constraints loaded from a data file.</summary>
           <param name=Path'>The path to the file containing the vector data.</param>
           <param name=SkipHeader'>Indicates whether or not to skip the first row of the file data when reading.</param>
           <param name=Col'>Indicates the column to use for the vector data .</param>
            """
        VF.AddXmlDoc helpText

        let pathParam = ProvidedStaticParameter("Path", typeof<string>)
        let skipHeaderParam = ProvidedStaticParameter("SkipHeader", typeof<bool>, true)
        let columnParam = ProvidedStaticParameter("Column", typeof<int>, 0)

        do VF.DefineStaticParameters([pathParam; skipHeaderParam; columnParam], fun name args ->
            let path = args.[0] :?> string
            let skipHeader = args.[1] :?> bool
            let col = args.[2] :?> int
            let data = FileData.readCsvSingleCol<single> path skipHeader col |> Seq.toArray
            let dataExpr = Expr.Value data
            let n = data.Length
            let tp = Array.concat [[|typeof<single>|];getIntBase10TypeArray(n, 10)]
            let g = typedefof<Vector<_,_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(tp)
            let provided = ProvidedTypeDefinition(asm, ns, name, Some g, false)
            provided.AddXmlDoc <| (sprintf "<summary>Floating-point vector of length %d with type-level dimension constraints.</summary>" <| n)   
            let ctor = ProvidedConstructor([], invokeCode = fun args -> 
                <@@ Activator.CreateInstance(typedefof<Vector<_,_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(Array.concat [[|typeof<single>|];getIntBase10TypeArray(n, 10)]), (%%(dataExpr) : single[])) @@>)
          
            provided.AddMember(ctor)
          
            provided
        )

        let M = ProvidedTypeDefinition(asm, ns, "Mat", Some typeof<Matrix<_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_>>, false)   
        let helpText = 
            """<summary>Floating-point matrix with type-level dimension constraints.</summary>
           <param name=Rows'>The number of rows in the matrix.</param>
           <param name=Cols'>The number of columns in the matrix.</param>
            """
        M.AddXmlDoc helpText

        let dim0Param = ProvidedStaticParameter("Rows", typeof<int>)
        let dim1Param = ProvidedStaticParameter("Cols", typeof<int>)

        do M.DefineStaticParameters([dim0Param; dim1Param], fun name args ->
            let d0 = args.[0] :?> int
            let d1 = args.[1] :?> int
            let tp = Array.concat [[|typeof<single>|];getIntBase10TypeArray(d0, 10);getIntBase10TypeArray(d1, 10)]
            let g = typedefof<Matrix<_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(tp)
            let provided = ProvidedTypeDefinition(asm, ns, name, Some g, false)
            provided.AddXmlDoc <| (sprintf "<summary>Floating-point matrix of %d rows and %d columns with type-level dimension constraints.</summary>" d0 d1)   
            
            let ctor = ProvidedConstructor([], invokeCode = fun args -> 
                <@@ Activator.CreateInstance(typedefof<Matrix<_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(Array.concat [[|typeof<single>|];getIntBase10TypeArray(d0, 10); getIntBase10TypeArray(d1, 10)])) @@>)
            let ctor2 = ProvidedConstructor([ ProvidedParameter("items",typeof<single[,]>) ], invokeCode = fun args -> 
                <@@ Activator.CreateInstance(typedefof<Matrix<_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(Array.concat [[|typeof<single>|];getIntBase10TypeArray(d0, 10); getIntBase10TypeArray(d1, 10)]), (%%(args.[0]) : single[,])) @@>)
            let ctor3 = ProvidedConstructor([ ProvidedParameter("value",typeof<single>) ], invokeCode = fun args -> 
                <@@ Activator.CreateInstance(typedefof<Matrix<_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(Array.concat [[|typeof<single>|];getIntBase10TypeArray(d0, 10); getIntBase10TypeArray(d1, 10)]), (%%(args.[0]) : single)) @@>)

            provided.AddMember(ctor)
            provided.AddMember(ctor2)
            provided.AddMember(ctor3)
            provided
        )

        let MF = ProvidedTypeDefinition(asm, ns, "MatF", Some typeof<Matrix<_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_>>, false)   
        let helpText = 
            """<summary>Floating-point matrix with type-level dimension constraints loaded from a data file.</summary>
           <param name=Path'>The path to the file containing the matrix data.</param>
           <param name=SkipHeader'>Indicates whether or not to skip the first row of the file data when reading.</param>
           <param name=ColStart'>Indicates the column to start reading the matrix data .</param>
           <param name=ColEnd'>Indicates the column to end reading the matrix data .</param>
            """
        MF.AddXmlDoc helpText

        let pathParam = ProvidedStaticParameter("Path", typeof<string>)
        let skipHeaderParam = ProvidedStaticParameter("SkipHeader", typeof<bool>, true)
        let columnStartParam = ProvidedStaticParameter("ColStart", typeof<int>, 0)
        let columnEndParam = ProvidedStaticParameter("ColEnd", typeof<int>)

        do MF.DefineStaticParameters([pathParam; skipHeaderParam; columnStartParam; columnEndParam], fun name args ->
            let path = args.[0] :?> string
            let skipHeader = args.[1] :?> bool
            let colstart = args.[2] :?> int
            let colend = args.[3] :?> int
            let _data = FileData.readCsvCols<single> path skipHeader (seq {colstart..colend}) |> Seq.toArray
            let data = Array2D.init (_data.GetLength(0)) (colend - colstart + 1) (fun i j -> _data.[i].[j])
            let dataExpr = Expr.Value data
            let d0 = data.GetLength(0)
            let d1 = ((colend - colstart) + 1)
            let tp = Array.concat [[|typeof<single>|];getIntBase10TypeArray(d0, 10);getIntBase10TypeArray(d1, 10)]
            let g = typedefof<Matrix<_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(tp)
            let provided = ProvidedTypeDefinition(asm, ns, name, Some g, false)
            provided.AddXmlDoc <| (sprintf "<summary>Floating-point matrix of %d rows and %d columns with type-level dimension constraints.</summary>" d0 d1)   
            
            let ctor = ProvidedConstructor([], invokeCode = fun args -> 
                <@@ Activator.CreateInstance(typedefof<Matrix<_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(Array.concat [[|typeof<single>|];getIntBase10TypeArray(d0, 10); getIntBase10TypeArray(d1, 10)]), (%%(dataExpr) : single[,])) @@>)
          
            provided.AddMember(ctor)
            provided
        )

        
        [V; M; VF; MF]

    do
        this.AddNamespace(ns, createTypes())
        
[<TypeProviderAssembly>]
do ()
