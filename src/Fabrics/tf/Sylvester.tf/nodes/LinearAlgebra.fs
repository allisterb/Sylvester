namespace Sylvester.tf

[<AutoOpen>]
module LinearAlgebra =
    let matmul (l:Node) (r:Node) = Node(l.TensorGraph, ops(l).MatMul(l.Op.[0], r.Op.[0]), [])
       