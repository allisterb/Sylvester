using System;
using System.Collections.Generic;
using System.Linq;

using Xunit;
using Sylvester.Data;

namespace Sylvester.Tests.Data
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

        [Fact]
        public void CanParseFileFromUrl()
        {
            CsvFile file = new CsvFile("https://forge.scilab.org/index.php/p/rdataset/source/file/master/csv/datasets/Titanic.csv");
            Assert.NotEmpty(file.Fields);
            file.Fields.RemoveAt(1);
            Assert.Equal("Sex", file[1].Label);
        }

        [Fact]
        public void CanBatchParseFile()
        {
            var file = new CsvFile("transfusion.data");
            foreach(var field in file)
            {
                field.Type = typeof(int);
            }
            file.BatchSize = 70;
            file.Parse(0);
            Assert.Equal(748, file.Fields[0].BatchData.Sum(b => b.Length));
            Assert.Equal(2250, file.Fields[2].BatchData[8].GetValue(1)); //row 562 3rd field
        }
       
    }
}
