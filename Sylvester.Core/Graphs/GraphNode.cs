// Based on: https://msdn.microsoft.com/en-us/library/ms379574(v=vs.80).aspx
using System;
using System.Collections.Generic;
using System.Text;

namespace Sylvester.Graphs
{
    public class GraphNode<T> : Node<T>
    {
        public List<int> Costs { get; protected set; } = new List<int>();


        public GraphNode(T value) : base(value) { }

        public GraphNode(T value, List<Node<T>> neighbors) : base(value, neighbors) { }

    }
}
