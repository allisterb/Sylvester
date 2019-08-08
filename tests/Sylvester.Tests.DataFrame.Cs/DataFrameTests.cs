using System;
using System.Collections.Generic;

using Microsoft.CSharp;
using Microsoft.CSharp.RuntimeBinder;
using System.Linq;
using Xunit;

using Sylvester.DataFrame.Dynamic;
using Sylvester.Data;

namespace Sylvester.Tests.Data
{
    public class CsDataFrameTests
    {
        [Fact]
        public void CanConstructFrame()
        {
            dynamic f = new Frame();
            dynamic g = new Sn<float>(new[] { 1.0f, 2.0f, 4.0f }, "A");
            f.S1 = new Sn<float>(new[] { 1.0f, 2.0f, 4.0f });
            var s1 = f.S1;
            Assert.NotNull(s1);
            Assert.Equal("S1", s1.Label);
            dynamic f2 = new Frame(g);
            Assert.NotNull(f2.A);
            Sn<float> s2 = f2.A;
            Assert.NotNull(s2);
            Assert.Equal("A", s2.Label);
            Assert.Throws<FrameUnrestrictedMembersNotEnabledException>(() => f.P = "foo");
            Assert.Throws<RuntimeBinderException> (() => f.P);
        }

        [Fact]
        public void CanConstructFrameFromCsv()
        {
            CsvFile file = new CsvFile("mtcars.csv");
            Assert.NotEmpty(file.Fields);
            Assert.Equal("Field0", file.Fields[0].Label);
            file[1].Type = typeof(Single);
            dynamic f = new Frame(file);
            Assert.NotNull(f.mpg);
            dynamic g = f.SerF(f.mpg);
            Assert.NotNull(g.mpg);
            dynamic e = f.SerExF(0);
            Assert.NotNull(e.disp);

            CsvFile trans = new CsvFile("transfusion.data");
            Assert.NotEmpty(trans);
            Assert.Equal("Frequency (times)", trans[1].Label);
            foreach (var transf in trans.Fields)
            {
                transf.Type = typeof(int);
                transf.Label = transf.Label.Replace(" ", "");
            }
            trans[0].Label = "Recency";
            trans.BatchSize = 70;
            dynamic dt = new Frame(trans);
            Assert.Equal(23, dt.Recency[729]);
            Assert.Equal(16, dt[714].Recency);
        }

        [Fact]
        public void CanConstructFrameFromAnonymous()
        {
            dynamic z = new Frame(new Array[] { new string[100], new int[100], new DateTime[100], new bool[100] }, new { Name = "John Die", Age = 0, Birthday = DateTime.Now, Active = false }); 
            Assert.NotNull(z.Name);
            Assert.NotNull(z.Birthday);
            z.Name[0] = "John Doe";
            z.Birthday[0] = DateTime.Now;
            _ = Assert.IsType<string>(z.Name[0]);
            Assert.IsType<DateTime>(z.Birthday[0]);
            Assert.False(z.Active[0]);
            var r = z[0];
            Assert.NotNull(r.Name);
            Assert.NotNull(r.Birthday);
        }

        [Fact]
        public void CanAddSeriesToFrame()
        {
            dynamic f = new Frame(new Sn<double>(new[] { 1.0, 3.0, 5.0, float.NaN, 6.0, 8.0 }, "Age"));
            Assert.Equal(3.0, f.Age[1]);
            var r2 = f[2];
            Assert.NotNull(r2.Age);
            Assert.Equal(5.0, r2.Age);
            f.Children = new Sn<double>(new[] { 1.0, 3.0, 5.0, float.NaN, 6.0, 8.0 });
            Assert.NotNull(f.Children);
            Assert.Equal("Children", f.Children.Label);
        }

        [Fact]
        public void CanQueryFrameUsingLinq()
        {
            Frame f = new Frame(new CsvFile("mtcars.csv"));

            var q =
                from row in f
                select row["mpg"];
            Assert.NotEmpty(q);

            var q2 =
                from row in f
                select (row.Ser("mpg").Add(("Foo1", 1)));
            Assert.NotEmpty(q2);
            var v = q2.ToFrameV();
            Assert.NotEmpty(v);

        }
    }
}
