namespace Sylvester.tf

open System

type Scope(graph: ITensorGraph, scope:string, ?sub:bool) =
    let parent = graph.NameScope
    let isChild = defaultArg sub false
    let s = 
        if String.IsNullOrEmpty(parent) then 
            scope  
        else if isChild then parent + "/" + scope else scope
    do graph.NameScope <-s
    interface IDisposable with 
        member x.Dispose() = do graph.NameScope <- parent

[<AutoOpen>]
module Scope = 
    
    let scope name = new Scope(defaultGraph, name)
    
    let subscope name = new Scope(defaultGraph, name, true)

    let ends (s:Scope) = let _s = s :> IDisposable in _s.Dispose()
    
    
