namespace Sylvester.tf

open System
open System.Collections.Generic;
open System.Runtime.CompilerServices

open TensorFlow

open Sylvester
open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10
open Sylvester.Collections
open Sylvester.Graphs
open Sylvester.Tensors

type Scope(graph: ITensorGraph, name:string) =
    let parent = graph.NameScope
    do graph.NameScope <- parent + "/" + name
    interface IDisposable with 
        member x.Dispose() = do graph.NameScope <- parent

[<AutoOpen>]
module Scope = 
   
    let scope name = new Scope(defaultGraph, name)
    
    let ends (s:Scope) = let _s = s :> IDisposable in _s.Dispose()
    
    
