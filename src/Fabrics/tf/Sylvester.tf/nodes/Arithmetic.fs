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
module Arithmetic =

    type TensorGraph<'input, 'output when 'input :> Number and 'output :> Number> with
        member graph.Add(l:Tensor<'r, 't>, r:Tensor<'r, 't>) = Node(graph, "Add", graph._Graph.Add(l.Output, r.Output), [l:>Edge;r:>Edge])
        member graph.Sub(l:Tensor<'t>, r:Tensor<'t>) = Node(graph, "Sub", graph._Graph.Sub(l.Output, r.Output), [l:>Edge;r:>Edge])
        member graph.Mul(l:Tensor<'t>, r:Tensor<'t>) = Node(graph, "Mul", graph._Graph.Mul(l.Output, r.Output), [l:>Edge;r:>Edge])
