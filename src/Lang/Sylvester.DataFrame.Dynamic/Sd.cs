using System;
using System.Collections.Generic;
using System.Linq;


namespace Sylvester.Data
{
    public class Sd : Sv<DateTime>
    {
        public Sd(DateTime[] data, string label, DateTime defaultVal = default) : base(data, label, defaultVal)
        { }

        public Sd(DateTime[] data) : this(data, "") { }

        public Sd(IEnumerable<DateTime> data) : this(data.ToArray()) { }


        public static implicit operator Sd(DateTime[] array) => new Sd(array);
    }
}
