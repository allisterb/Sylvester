using System;
using System.Linq;
using System.Linq.Expressions;
using System.Collections.Generic;
using System.Reflection;
using System.Text;
using Sawmill.Expressions;
using Xunit;

namespace Sylvester.Tests
{
    public class ExtensionsTests
    {
        [Fact]
        public void CanFlatten()
        {
            int[,] a = { { 1, 2, 3, 4 }, { 0, 0, 5, 7 } };
            IEnumerable<int> f = a.Flatten<int>();
            Assert.Equal(8, f.Count());
        }
    }
}
