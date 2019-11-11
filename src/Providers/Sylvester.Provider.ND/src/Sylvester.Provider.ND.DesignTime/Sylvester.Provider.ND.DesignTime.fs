module Sylvester.Provider.NDImplementation

open System
open System.Collections.Generic
open System.IO
open System.Linq
open System.Reflection

open FSharp.Quotations
open FSharp.Core.CompilerServices
open ProviderImplementation
open ProviderImplementation.ProvidedTypes

open Numpy
open Numpy.Models

open Sylvester.Arithmetic
open Sylvester.Arithmetic.Base10
open Sylvester.Arithmetic.N10

[<TypeProvider>]
type NDArrayProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config, assemblyReplacementMap=[("Sylvester.Provider.ND.DesignTime", "Sylvester.Provider.ND.Runtime")], addDefaultProbingLocation=true)

    let ns = "Sylvester.Fabric.Keras"
    let asm = Assembly.GetExecutingAssembly()

    
    let createTypes () =
        let ND = ProvidedTypeDefinition(asm, ns, "ND", Some typeof<NDarray>)
        let helpText = 
            """<summary>N-dimensional tensor with type-level dimension constraints for use with the Keras fabric.</summary>
           <param name=dim0'>The length first dimension.</param>
            """
        let dtype = ProvidedStaticParameter("dtype", typeof<Dtype>)  
        let dp0 = ProvidedStaticParameter("dim0", typeof<int>)
        let dp1 = ProvidedStaticParameter("dim1", typeof<int>)
        //let dp2 = ProvidedStaticParameter("dim2", typeof<int>)
        //let dp3 = ProvidedStaticParameter("dim3", typeof<int>)
        //let dp4 = ProvidedStaticParameter("dim4", typeof<int>)
        //let dp5 = ProvidedStaticParameter("dim5", typeof<int>)
        //let dp6 = ProvidedStaticParameter("dim6", typeof<int>)
        //let dp7 = ProvidedStaticParameter("dim7", typeof<int>)
        //let dp8 = ProvidedStaticParameter("dim8", typeof<int>)
        //let dp9 = ProvidedStaticParameter("dim9", typeof<int>)
        //let dp10 = ProvidedStaticParameter("dim10", typeof<int>)
        //let dp11 = ProvidedStaticParameter("dim11", typeof<int>)
        //let dp12 = ProvidedStaticParameter("dim12", typeof<int>)
        //let dp13 = ProvidedStaticParameter("dim13", typeof<int>)
        //let dp14 = ProvidedStaticParameter("dim14", typeof<int>)
        do ND.DefineStaticParameters([dtype; dp0; dp1], fun name args ->
            let _d0 = args.[1] :?> int
            let _d1 = args.[2] :?> int
            //let _d2 = args.[3] :?> int
            //let _d3 = args.[4] :?> int
            //let _d4 = args.[5] :?> int
            //let _d5 = args.[6] :?> int
            //let _d6 = args.[7] :?> int
            //let _d7 = args.[8] :?> int
            //let _d8 = args.[9] :?> int
            //let _d9 = args.[10] :?> int
            //let _d10 = args.[11] :?> int
            //let _d11 = args.[12] :?> int
            //let _d12 = args.[13] :?> int
            //let _d13 = args.[14] :?> int
            
            let dt0 = typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(_d0, 10))
            let dt1 = typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(_d1, 10))
            //let dt2 = typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(_d2, 10))
            //let dt3 = typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(_d3, 10))
            //let dt4 = typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(_d4, 10))
            //let dt5 = typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(_d5, 10))
            //let dt6 = typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(_d6, 10))
            //let dt7 = typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(_d7, 10))
            //let dt8 = typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(_d8, 10))
            //let dt9 = typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(_d9, 10))
            //let dt10 = typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(_d10, 10))
            //let dt11 = typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(_d11, 11))
            //let dt12 = typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(_d12, 10))
            //let dt13 = typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(_d13, 10))
        
            let dv0 = Activator.CreateInstance(dt0)
            let dv1 = Activator.CreateInstance(dt1)
            //let dv2 = Activator.CreateInstance(dt2)
            //let dv3 = Activator.CreateInstance(dt3)
            //let dv4 = Activator.CreateInstance(dt4)
            //let dv5 = Activator.CreateInstance(dt5)
            //let dv6 = Activator.CreateInstance(dt6)
            //let dv7 = Activator.CreateInstance(dt7)
            //let dv8 = Activator.CreateInstance(dt8)
            //let dv9 = Activator.CreateInstance(dt9)
            //let dv10 = Activator.CreateInstance(dt10)
            //let dv11 = Activator.CreateInstance(dt11)
            //let dv12 = Activator.CreateInstance(dt12);
            //let dv13 = Activator.CreateInstance(dt13);

            let dexpr0 = Expr.Value(dv0, dt0)
            
            let d0 = ProvidedProperty("dim0", dt0, getterCode = fun args -> <@@ %%(dexpr0) @@>)
            // myType.AddMember(innerState)

            
            //provided.AddXmlDoc <| (sprintf "<summary>Floating-point matrix of %d rows and %d columns with type-level dimension constraints.</summary>" d0 d1)   
    
            //let ctor = ProvidedConstructor([], invokeCode = fun args -> 
            //    <@@ Activator.CreateInstance(typedefof<Matrix<_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(Array.concat [[|typeof<single>|];getIntBase10TypeArray(d0, 10); getIntBase10TypeArray(d1, 10)]), (%%(dataExpr) : single[,])) @@>)
  
            //provided.AddMember(ctor)
            let ndarrayExpr = Expr.Value(np.zeros(_d0), typeof<NDarray>)
            let ctor = ProvidedConstructor([], invokeCode = fun args -> <@@ %%(ndarrayExpr) :> NDarray @@>)

            let provided = ProvidedTypeDefinition(asm, ns, name, Some typeof<NDarray>, false)
            provided.AddMember(d0)
            provided.AddMember(ctor)
            provided
        )
        // myType.AddMember(ctor)

        // let ctor2 = ProvidedConstructor([ProvidedParameter("InnerState", typeof<string>)], invokeCode = fun args -> <@@ (%%(args.[0]):string) :> obj @@>)
        // myType.AddMember(ctor2)

        // let innerState = ProvidedProperty("InnerState", typeof<string>, getterCode = fun args -> <@@ (%%(args.[0]) :> obj) :?> string @@>)
        // myType.AddMember(innerState)

        // let meth = ProvidedMethod("StaticMethod", [], typeof<DataSource>, isStatic=true, invokeCode = (fun args -> Expr.Value(null, typeof<DataSource>)))
        // myType.AddMember(meth)

        [ND]

    do
        this.AddNamespace(ns, createTypes())

[<TypeProviderAssembly>]
do ()
