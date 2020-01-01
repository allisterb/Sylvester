namespace Sylvester.tf

open System
open System.Collections.Generic;
open System.Runtime.CompilerServices



open Sylvester
open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10
open Sylvester.Collections
open Sylvester.Graphs

[<AutoOpen>]
module Inputs =
    let unknownInput<'t when 't : struct and 't :> ValueType and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable>(name:string, shape:int64[] option) =   
        new Tensor<'t>(defaultGraph, name, Node(defaultGraph, "Placeholder", defaultTFGraph.Placeholder(dataType<'t>, defaultArg shape null), []), 0, defaultArg shape null)

    let scalarInput<'t when 't : struct and 't :> ValueType and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable>(name:string) =   
        new Scalar<'t>(defaultGraph, name, Node(defaultGraph, "Placeholder", defaultTFGraph.Placeholder(dataType<'t>), []), 0)

    let vectorInput<'n, 't when 'n :> Number and  't : struct and 't :> ValueType and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable>(name:string) =   
        new Vector<'n, 't>(defaultGraph, name, Node(defaultGraph, "Placeholder", defaultTFGraph.Placeholder(dataType<'t>, [|number<'n>.Val|]), []), 0)

        //member graph.Input<'r, 't when 'r :> Number and 't:> ValueType and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable>(name:string) = 
        //    let shape = Array.create number<'r>.IntVal 0L
        //    new Tensor<'r, 't>(graph, name, new Node(graph, "Placeholder", graph._Graph.Placeholder(dataType<'t>, shape), []), 0, shape)
            