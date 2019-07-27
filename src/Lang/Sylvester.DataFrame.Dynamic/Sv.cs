using System;
using System.Collections;
using System.Collections.Generic;
using System.Text;

namespace Sylvester
{
    public class Sv<T> : FrameC<T>, IEnumerable<T> where T : struct, IEquatable<T>, IComparable<T>
    {
        public Sv(T[] data, string label, object defaultVal = default) : base(label, defaultVal)
        {
            Data = data;
        }

        public Sv(T[] data) : this(data, "") { }

        public T[] Data { get; }

        public override int Length => Data.Length;

        public override bool SetVal(int index, dynamic value)
        {
            Data[index] = value;
            return true;
        }

        public override T this[int index]
        {
            get => Data[index];
            set
            {
                SetVal(index, value);
            }
        }

        public override ref T Ref(int index) => ref Data[index];

        public override IEnumerator GetEnumerator() => Data.GetEnumerator();

        IEnumerator<T> IEnumerable<T>.GetEnumerator() => (Data as IEnumerable<T>).GetEnumerator();


        public override ISeries Append(params dynamic[] values)
        {
            T[] a = new T[Data.Length + values.Length];
            Data.CopyTo(a, 0);
            values.CopyTo(a, Length);
            return new Sv<T>(a, Label);
        }

        public override ISeries Clone(string label) => new Sv<T>(Data, label);
    }
}
