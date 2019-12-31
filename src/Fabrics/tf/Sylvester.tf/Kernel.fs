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

type Kernel<'input, 'output when 'input :> Number and 'output :> Number>(graph:TensorGraph<'input, 'output>) =
    inherit Api()

    member x.Graph = graph

    member x.Run(inputVals: VArray<'input, TF_Tensor>, outputVals: VArray<'output, TF_Tensor>, targets: TF_Operation[]) =
        let s = TF_Session.New(x.Graph._Graph)
        let _inputs = x.Graph.Inputs.Map(fun i -> i.Output)._Array
        let _inputVals = inputVals._Array
        let _outputs = x.Graph.Outputs.Map(fun i -> i.Output)._Array
        let _outputVals = outputVals._Array
        s.Run(_inputs, _inputVals, _outputs, _outputVals)



