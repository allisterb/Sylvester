namespace Sylvester.tf

open System
open System.IO

open Google.Protobuf
open Google.Protobuf.Collections
open Satsuma
open Tensorflow

open Sylvester

/// Directed acyclic graph of operation nodes
type OpGraph (stream:Stream) = 
    inherit AbstractGraph()    
    let pbdef = GraphDef.Parser.ParseFrom(CodedInputStream.CreateWithLimits(stream, 256 * 1024 * 1024, 100))
    do if pbdef.Node.Count > 0 then failwith "This graph definition is empty."
    let z = new NodeDef()
    
    override x.Initialized = pbdef.IsInitialized() && pbdef.Node.Count > 0
    
    
    member x.AddNode(n:NodeDef) = base.AddNode(n.GetHashCode() |> Convert.ToInt64)

    //member x.AddArc(head:NodeDef, tail:NodeDef) = 
        //let u = 
      //  base.AddArc (head.GetHashCode() |> Convert.ToInt64)
    
    //member 
    //do if pbdefn.
    
    //member val PbDefn = 
    
    //member x.Ids = x.PbDefn.Node |> Seq.map (fun n -> n.Name.GetHashCode(), n.Name) |> Map.ofSeq
    