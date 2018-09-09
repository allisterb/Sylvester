// Based on: https://msdn.microsoft.com/en-us/library/ms379572(v=vs.80).aspx
using System;
using System.Collections.Generic;
using System.Text;

namespace Sylvester.Graphs
{
    public class Node<T>
    {
        public T Value { get; protected set; }

        public List<Node<T>> Neighbors { get; protected set; }

        public Node(T value, List<Node<T>> neighbors)
        {
            this.Value = value;
            this.Neighbors = neighbors;
        }

        public Node(T value) : this(value, new List<Node<T>>()) { }
    }
}
