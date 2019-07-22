using System;
using Xunit;

using Sylvester.DataFrame.Dynamic;

namespace Sylvester.Tests
{
    public class DataFrameTests
    {
        [Fact]
        public void CanConstructFrame()
        {
            dynamic f = new Frame();
            dynamic g = new Sn<float>(new[] { 1.0f, 2.0f, 4.0f }, "A");
            float r = g[0];
            f.S1 = new Sn<float>(new[] { 1.0f, 2.0f, 4.0f }, "A");
            dynamic f2 = new Frame(g);
            Assert.NotNull(f2.A);
            Sn<float> s = f2.A;
            Assert.NotNull(s);
            Assert.Equal("A", s.Label);
        }
    }
}
