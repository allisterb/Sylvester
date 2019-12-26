namespace Sylvester.tf

open TensorFlow

open Sylvester
open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10
open Sylvester.Tensors
open Sylvester.Graphs

[<AbstractClass>]
type BaseTensor(graph: IGraph) = 
    inherit Api()
    interface IUnknownShape with  
        member val Rank:Option<int> = None with get, set
        member val Dims:Option<int[]> = None with get, set        

