namespace Sylvester.tf

open System.IO
open Sylvester

open Google.Protobuf
open Google.Protobuf.Collections
open Tensorflow

open Satsuma

type GraphDefn (stream:Stream) = 
    inherit Api()
    let pbdefn = GraphDef.Parser.ParseFrom(CodedInputStream.CreateWithLimits(stream, 256 * 1024 * 1024, 100))
    let graph = new CustomGraph()
    let ids = Seq.iter (fun n -> graph.Nodes.) (defn.Node)
    //Seq.iter (fun n:NodeDef -> n.) def.