using System;

using Microsoft.CSharp;
using Microsoft.CSharp.RuntimeBinder;
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
            f.S1 = new Sn<float>(new[] { 1.0f, 2.0f, 4.0f }, "A");
            var s1 = f.S1;
            Assert.NotNull(s1);
            Assert.Equal("A", s1.Label);
            dynamic f2 = new Frame(g);
            Assert.NotNull(f2.A);
            Sn<float> s2 = f2.A;
            Assert.NotNull(s2);
            Assert.Equal("A", s2.Label);
            Assert.Throws<FrameUnrestrictedMembersNotEnabledException>(() => f.P = "foo");
            Assert.Throws<RuntimeBinderException> (() => f.P);
        }
    }
}
