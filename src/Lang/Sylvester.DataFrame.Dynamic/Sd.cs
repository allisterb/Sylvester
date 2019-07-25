using System;
using System.Collections.Generic;
using System.Text;

namespace Sylvester
{
    public class Sd : Sv<DateTime>
    {
        public Sd(DateTime[] data, string label, DateTime defaultVal = default) : base(data, label, defaultVal)
        { }
    }
}
