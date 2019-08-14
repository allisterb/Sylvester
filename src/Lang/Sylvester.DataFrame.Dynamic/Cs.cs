using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Sylvester.Data
{
    public class Cs : Cr<string>
    {
        public Cs(string[] data, string label, string defaultVal = "") : base(data, label, defaultVal) {}

        public Cs(string[] data) : this(data, "") { }

        public Cs(IEnumerable<string> data) : this(data.ToArray()) { }

        public static implicit operator Cs(string[] array) => new Cs(array);

    }
}
