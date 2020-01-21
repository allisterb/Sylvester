namespace Sylvester.tf

open System
open System.Collections.Generic
open System.IO

open Google.Protobuf
open Google.Protobuf.Collections


open Sylvester

/// Directed graph of operation nodes
type GraphDef (stream:Stream) as this = 
    inherit NameScopedGraph()     
    let pbdef = Tensorflow.GraphDef.Parser.ParseFrom(CodedInputStream.CreateWithLimits(stream, 256 * 1024 * 1024, 100))
    do if pbdef.Node.Count = 0 then failwith "This graph definition is empty."
    do pbdef.Node |> Seq.iter (fun n -> this.AddNode(n.Name))
    
    new(path:string) = GraphDef(File.OpenRead(path))     