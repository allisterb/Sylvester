using System;

namespace Sylvester.Notation
{
    public class Name
    {
        public string Label { get; protected set; }

        public int BaseIndex { get; protected set; }

        public Name(int baseIndex) : this(new string(Convert.ToChar(baseIndex), 1))
        {
        }

        public Name(string label, int index = 0)
        {
            Label = label;
            BaseIndex = index;
        }

        public static Name Base(int b) => new Name(b);

        public static implicit operator string(Name n) => n.Label;

        public static implicit operator Name(string s) => new Name(s);
    }
}