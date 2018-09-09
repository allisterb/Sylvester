// Based on: https://msdn.microsoft.com/en-us/library/ms379574(v=vs.80).aspx
using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;

namespace Sylvester.Graphs
{
    public class Graph<T> : IEnumerable<T>
    {
        protected List<Node<T>> Nodes { get; set; }


        public Graph(List<Node<T>> nodes)
        {
            Nodes = nodes;
        }

        public Graph(IEnumerable<Node<T>> nodes)
        {
            Nodes = nodes.ToList();
        }

        public Graph() : this(new List<Node<T>>()) { }


        public void AddNode(GraphNode<T> node)
        {
            // adds a node to the graph
            Nodes.Add(node);
        }

        public void AddNode(T value)
        {
            // adds a node to the graph
            Nodes.Add(new GraphNode<T>(value));
        }

        public void AddDirectedEdge(GraphNode<T> src, GraphNode<T> dest, int cost)
        {
            src.Neighbors.Add(dest);
            src.Costs.Add(cost);
        }

        public void AddUndirectedEdge(GraphNode<T> src, GraphNode<T> dest, int cost)
        {
            src.Neighbors.Add(dest);
            src.Costs.Add(cost);

            dest.Neighbors.Add(src);
            dest.Costs.Add(cost);
        }

    
        public bool Remove(T value)
        {
            // first remove the node from the nodeset
            Node<T> nodeToRemove = Nodes.SingleOrDefault(n => n.Value.Equals(value));
            int nid;
            if (nodeToRemove == null)
            {
                return false;
            }
            {
                nid = Nodes.IndexOf(nodeToRemove); 
            }

            // otherwise, the node was found
            Nodes.Remove(nodeToRemove);

            // enumerate through each node in the nodeSet, removing edges to this node
            foreach (GraphNode<T> gnode in Nodes)
            {
                // remove the reference to the node and associated cost
                gnode.Neighbors.Remove(nodeToRemove);
                gnode.Costs.RemoveAt(nid);
            }
            return true;
        }

        public IEnumerator<T> GetEnumerator()
        {
            foreach (var n in Nodes)
            {
                yield return (n.Value);
            }
        }

        IEnumerator IEnumerable.GetEnumerator() => this.GetEnumerator();

    }
}
