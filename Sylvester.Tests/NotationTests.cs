using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;

using System.Text;

using Xunit;

using Sylvester.Notation;

namespace Sylvester.Tests
{
    public class NotationTests
    {
        [Fact]
        public void CanConstructTensorNotation()
        {
            Tensor A = Tensor.TwoD("A", (7,7), "a", out Index a, out Index b);
            Assert.Equal("A", A.Name);
            Assert.Equal(2, A.Rank);
            Assert.Equal("a", a.Name);
            Assert.Equal("b", b.Name);
            var A1 = A[a, b];
            Assert.IsType<IndexExpression>((Expression) A1);

            var B = Tensor.ThreeD("B", (5, 6, 7), "i", out var ijk);
            var (i, j, k) = ijk;
            Assert.Equal("i", i.Name);
            Assert.Equal("j", j.Name);
            Assert.Equal("k", k.Name);
            Assert.Equal(1, j.Order);

            Assert.IsType<IndexExpression>((Expression) B[i]);
            Assert.IsType<IndexExpression>((Expression) B[ijk]);
            B[ijk] = A[i];
            Assert.True(B.IsDefined);
            Assert.True(B.ContractionDefinition.IndexSet[1].Equals(j));
        }

        [Fact]
        public void CanUseFluentTensorConstruction()
        {
            Tensor D = Tensor.TwoD("D", (11, 12))
                .With(out Tensor E)
                .With(out Tensor F, 4, 3);

            Assert.Equal("D", D.Name);
            Assert.Equal(12, D.Dimensions[1]);

            Assert.Equal("E", E.Name);
            Assert.Equal(12, E.Dimensions[1]);

            Assert.Equal("F", F.Name);
            Assert.Equal(4, F.Dimensions[0]);
            Assert.Equal(3, F.Dimensions[1]);
            Assert.Equal(2, F.Rank);
            Assert.Throws<ArgumentException>(() => F.With(out Tensor G, 3, 2, 1));
        }

        [Fact]
        public void CanUseTupleTensorConstructors()
        {
            var (H, I, J) = Tensor.ThreeD("H", (2, 2, 2)).Three();
            Assert.Equal("H", H.Label);
            Assert.Equal("I", I.Label);
            Assert.Equal("J", J.Label);
            Assert.Equal(3, J.Rank);
            var (M1, M2, M3) = Tensor.ThreeD((2, 2, 2)).Three("M1", "M2", "M3");
            Assert.Equal("M1", M1.Name);
            Assert.Equal(3, M1.Rank);
            Assert.Equal("M3", M3.Name);
            Assert.Equal(8, M3.ElementCount);
        }

        [Fact]
        public void CanGenerateTermNames()
        {
            var B = Tensor.FiveD(tn.B, (4, 5, 6, 7, 8));
            Assert.Equal("B", B.Name);
            var I = new IndexSet(B, "m0", B.Dimensions);
            Assert.Equal("m0", I[0].Name);
            var J = new IndexSet(B, "i0", B.Dimensions);
            Assert.Equal("i0", J[0].Name);
            var (m0, m1, m2, m3, m4) = I;
            Assert.Equal("m4", m4.Name);

            var (N1, N2, N3, N4) = Tensor.FourD("N1", (2, 7, 9, 6), "n1", out Index n1, out Index n2, out Index n3, 
                out Index n4).Four();
            Assert.Equal("N2", N2.Name);
            Assert.Equal("N4", N4.Name);
            Assert.Equal("n3", n3.Name);
            Assert.Equal("n4", n4.Name);
        }

        [Fact]
        public void CanAssignTensorExpression()
        {
            var (A, B, C) = Tensor.TwoD("A", (4,3), "a", out Index a, out Index b).Three();
            C[a,b] = B[a,b] * C[b,a];
            Assert.True(C.IsDefined);
        }

        [Fact]
        public void CanUseDimensionExpressions()
        {
            var (A, B, C) = Tensor.TwoD((4, 3), out Index i, out Index j).Three();
            var (M, N, O) = Tensor.ThreeD("M", (7, 9, 6), "m", out Index m, out Index n, out Index o).Three();

            A[i,j] = (B[i + 2, j] * C[i + 2, j]);
            Assert.True(A.IsDefined);
            Assert.True(A.ContractionDefinition.Expression.IndexParameters.Count == 4);
            List<Index> indices = A.ContractionDefinition.Expression.IndexParameters;
            M[n] = A[m]; 
            Assert.True(M.IsDefined);
            //Assert.NotNull(M.ContractionDefinition.Expression.Bounds);
        }

        [Fact]
        public void CanConstructVectorExpression()
        {
            var (V0, V1) = new Vector("V0", 5).Two();
            Assert.Equal("V0", V0.Name);
            Assert.Equal("V1", V1.Name);
            var (c0, c1, c2) = new Scalar("c0").Three();
            Assert.Equal("c1", c1.Name);
            V1.def = c0 * V0 + c1;
            Assert.True(V1.IsDefined);
            Assert.Equal(3, V1.ElementwiseDefinition.TensorReferences.Count);

            var (x, y) = new Vector("x", 2).Two();
            Assert.Equal("x", x.Name);
            Assert.Equal("y", y.Name);
            var (a, b) = new Scalar("a").Two();
            y.def = a * x + b;
            Assert.True(y.IsDefined);
            Assert.Equal(3, y.ElementwiseDefinition.TensorReferences.Count);
        }
    }
}
