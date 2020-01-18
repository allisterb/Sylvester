namespace Sylvester.tf

open System
open System.Collections.Generic
open System.IO

open Google.Protobuf
open Google.Protobuf.Collections
open Satsuma
open Tensorflow

open Sylvester

/// Directed graph of operation nodes
type GraphDefn (stream:Stream) as this = 
    inherit AbstractGraph()     
    let pbdef = GraphDef.Parser.ParseFrom(CodedInputStream.CreateWithLimits(stream, 256 * 1024 * 1024, 100))
    do if pbdef.Node.Count = 0 then failwith "This graph definition is empty."
    let nameIdMap = new Dictionary<string, int64>()
    let idNameMap = new Dictionary<int64, string>()
    do pbdef.Node |> Seq.iter (fun n ->
        let id = n.Name.GetHashCode() |> Convert.ToInt64
        let name = n.Name
        nameIdMap.Add(name, id)
        idNameMap.Add(id, name)
        this.AddNode(id) |> ignore
    )

    new(path:string) = GraphDefn(File.OpenRead(path))     