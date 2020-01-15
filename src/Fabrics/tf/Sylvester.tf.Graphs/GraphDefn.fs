namespace Sylvester.tf

open System.IO

open Google.Protobuf
open Google.Protobuf.Collections
open Satsuma

open Sylvester

type GraphDefn (stream:Stream) = 
    inherit AbstractGraph()    
    let pbdef = Tensorflow.GraphDef.Parser.ParseFrom(CodedInputStream.CreateWithLimits(stream, 256 * 1024 * 1024, 100))
    //do if pbdefn.
    
    //member val PbDefn = 
    
    //member x.Ids = x.PbDefn.Node |> Seq.map (fun n -> n.Name.GetHashCode(), n.Name) |> Map.ofSeq
    