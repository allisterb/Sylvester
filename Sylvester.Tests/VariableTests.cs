using System;
using System.Collections.Generic;
using System.Text;

using Xunit;

using Sylvester.Compiler;
using Sylvester.Notation;

namespace Sylvester.Tests
{
    public class VariableTests
    {
        [Fact]
        public void CanConstructVariable()
        {
            int[,] array1 = new int[,] { { 1, 2, 3 }, { 4, 5, 6 }, { 7, 8, 9 }, { 10, 11, 12 } };
            int[,] array2 = new int[,] { { 1, 2, 3 } };
            
            var A = Tensor.TwoD("A", (4, 3), "a", out Index a, out Index b);
            Assert.True(A.Var<int>(array1).Initialized);
            Assert.Throws<ArgumentException>(() => A.Var<int>(array2));
            Assert.True(A.Var(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12).Initialized);
            Assert.Throws<ArgumentException>(() => A.Var(1, 2, 3, 4, 5, 6, 7, 8, 9, 10));

            var V1 = new Vector(3).With(out Vector V2);
            Assert.True(V1.Var(6, 7, 8).Initialized);
            Assert.Throws<ArgumentException>(() => V1.Var(8));
        }

        [Fact]
        public void CanReadWriteVariable()
        {
            int[] array = new int[] { 9, 10, 11 };
            int[,] array1 = new int[,] { { 1, 2, 3 }, { 4, 5, 6 }, { 7, 8, 9 }, { 10, 11, 12 } };
            int[,] array2 = new int[,] { { 1, 2, 3 } };


            var V1 = new Vector(3).With(out Vector V2);
            var v1 = V1.Var(6, 7, 8);
            Assert.True(v1.Initialized);
            Assert.Equal(6, v1[0]);
            v1[0] = 5;
            Assert.Equal(5, v1[0]);
            Assert.Throws<IndexOutOfRangeException>(() => v1[4]);

            var va1 = Tensor.TwoD("A", (4, 3)).Var<int>(array1);
            Assert.True(va1.Initialized);
            Assert.Equal(4, va1[3]);
            Assert.Equal(11, va1[10]);

            va1.CopyFrom(1, 2, 3, 5, 6, 7, 8, 9, 10, 11, 12, 13);
            Assert.Equal(5, va1[3]);

        }
    }
}
