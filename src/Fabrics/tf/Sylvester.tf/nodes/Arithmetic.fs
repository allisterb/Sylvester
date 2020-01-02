namespace Sylvester.tf

[<AutoOpen>]
module Arithmetic =
    let add (l:Node) (r:Node) = Node(l.TensorGraph, "Add", ops(l).Add(l.Op.[0], r.Op.[0]), [])
       
    let sub (l:Node) (r:Node) = Node(l.TensorGraph, "Sub", ops(l).Add(l.Op.[0], r.Op.[0]), [])
    
    let mul (l:Node) (r:Node) = Node(l.TensorGraph, "Mul", ops(l).Add(l.Op.[0], r.Op.[0]), [])

    let div (l:Node) (r:Node) = Node(l.TensorGraph, "Div", ops(l).Add(l.Op.[0], r.Op.[0]), [])