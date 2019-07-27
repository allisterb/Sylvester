using System;
using System.Collections.Generic;

using Xunit;
using Sylvester.Data;

namespace Sylvester.Data.Tests
{
    public class CsvTests
    {
        [Fact]
        public void CanParseEntireFile()
        {
            CsvFile f = new CsvFile("mtcars.csv");
            Assert.NotEmpty(f.Fields);
            Assert.Equal("Field0", f.Fields[0].Label);
            f[1].Type = typeof(Single);
            f.Parse();
            Assert.NotEmpty(f.Fields[3].Data);
        }
    }
}
