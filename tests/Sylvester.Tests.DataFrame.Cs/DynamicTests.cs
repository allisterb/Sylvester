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
            f.X = "hh";
            var a = f[4];
        }
    }
}
