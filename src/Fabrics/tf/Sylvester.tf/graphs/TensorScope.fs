namespace Sylvester.tf

open System

type Scope(graph: ITensorGraph, scope:string) =
    let parent = graph.NameScope
    do graph.NameScope <- if String.IsNullOrEmpty(parent) then scope else parent + "/" + scope
    interface IDisposable with 
        member x.Dispose() = do graph.NameScope <- parent

[<AutoOpen>]
module Scope = 
    
    let scope name = new Scope(defaultGraph, name)
    
    let ends (s:Scope) = let _s = s :> IDisposable in _s.Dispose()
    
    
