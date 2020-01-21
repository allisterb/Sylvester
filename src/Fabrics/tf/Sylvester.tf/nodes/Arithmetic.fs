namespace Sylvester.tf

[<AutoOpen>]
module Arithmetic =
    let add (l:Node) (r:Node) = Node(l.Graph, ops(l).Add(l.Op.[0], r.Op.[0]), [])
       
    let sub (l:Node) (r:Node) = Node(l.Graph, ops(l).Add(l.Op.[0], r.Op.[0]), [])
    
    let mul (l:Node) (r:Node) = Node(l.Graph, ops(l).Add(l.Op.[0], r.Op.[0]), [])

    let div (l:Node) (r:Node) = Node(l.Graph, ops(l).Add(l.Op.[0], r.Op.[0]), [])