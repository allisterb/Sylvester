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
            List<string[]> rows = f.Parse(f.ReadEntireFile());
            Assert.NotEmpty(rows);
        }
    }
}
