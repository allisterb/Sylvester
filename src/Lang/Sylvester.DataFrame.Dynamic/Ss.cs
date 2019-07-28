using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Sylvester
{
    public class Ss : Sr<string>
    {
        public Ss(string[] data, string label, string defaultVal = "") : base(data, label, defaultVal) {}

        public Ss(string[] data) : this(data, "") { }

        public Ss(IEnumerable<string> data) : this(data.ToArray()) { }

    }
}
