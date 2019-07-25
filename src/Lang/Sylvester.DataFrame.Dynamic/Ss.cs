using System;
using System.Collections.Generic;
using System.Text;

namespace Sylvester
{
    public class Ss : Sr<string>
    {
        public Ss(string[] data, string label, object defaultVal = null) : base(data, label, defaultVal) {}
    }
}
