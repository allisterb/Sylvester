namespace Sylvester.Graphs

open System.Collections.Generic;
open Satsuma

/// Name-scoped abstract graph
type NSAbstractGraph(names:seq<string>) as this = 
    inherit AbstractGraph()

    let nameIdMap = names |> Seq.map (fun n -> n, n.GetHashCode() |> int64) |> Map.ofSeq
    let IdNameMap = names |> Seq.map (fun n -> n.GetHashCode() |> int64, n) |> Map.ofSeq
    do names |> Seq.iter(fun n -> this.AddNode(nameIdMap.[n]))