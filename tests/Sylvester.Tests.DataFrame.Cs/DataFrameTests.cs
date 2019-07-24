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

        [Fact]
        public void CanConstructFrameR()
        {
            dynamic f = new Frame(new Sn<double>(new[] { 1.0, 3.0, 5.0, float.NaN, 6.0, 8.0 }, "Age"));
            Assert.Equal(3.0, f.Age[1]);
            var r2 = f[2];
            Assert.NotNull(r2.Age);
            Assert.Equal(5.0, r2.Age);
            dynamic f2 = new Frame(new Sn<double>(new[] { 1.0, 3.0, 5.0, float.NaN, 6.0, 8.0 }, "Age"));
            f2.Children = new Sn<double>(new[] { 1.0, 3.0, 5.0, float.NaN, 6.0, 8.0 });
            Assert.NotNull(f2.Children);
            Assert.Equal("Children", f2.Children.Label);
        }
    }
}
