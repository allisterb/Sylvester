using System;
using System.Collections.Generic;
using System.Linq;


namespace Sylvester.Data
{
    public class Cd : Cv<DateTime>
    {
        public Cd(DateTime[] data, string label, DateTime defaultVal = default) : base(data, label, defaultVal)
        { }

        public Cd(DateTime[] data) : this(data, "") { }

        public Cd(IEnumerable<DateTime> data) : this(data.ToArray()) { }


        public static implicit operator Cd(DateTime[] array) => new Cd(array);
    }
}
