using System;
using System.Collections.Generic;
using System.Text;

using Sylvester.Notation;
using Sylvester.Diagrams;
using Sylvester.Trees;

using Xunit;

namespace Sylvester.Tests
{
    public class GraphDiagramTests
    {
        public GraphDiagramTests()
        {

        }

        [Fact]
        public void CanConstructGraphDiagram()
        {
            var A = Tensor.TwoD("A", (4, 3), "a", out Index a, out Index b);
            var B = Tensor.TwoD("B", (6, 7));
            var C = Tensor.TwoD("C", (8, 9));
            TensorExpression te = A[a, b];

            GraphDiagram d = new GraphDiagram(te.ToTree());
            Assert.Equal(4, d.Graph.NodeCount);
            Assert.Equal(3, d.Graph.EdgeCount);

            te = A[a, b] * C[a, b];
            d = new GraphDiagram(te.ToTree());
        }
    }
}
