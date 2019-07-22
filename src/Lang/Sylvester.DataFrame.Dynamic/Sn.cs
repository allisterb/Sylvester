using System;
using System.Collections.Generic;
using System.Text;

namespace Sylvester
{
    public class Sn<T> : Ser<T> where T : struct, IEquatable<T>, IComparable<T>, IConvertible, IFormattable
    {
        public Sn(T[] data, string label) : base(data, label) {}
    }
}
