using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Sylvester.Data
{
    public class Sn<T> : FrameC<T>, IEnumerable<T> where T : struct, IEquatable<T>, IComparable<T>, IConvertible
    {
        public Sn(T[] data, string label, T defaultVal = default) : base(label, defaultVal)
        {
            Data = data;
        }

        public Sn(T[] data) : this(data, "") { }

        public Sn(IEnumerable<T> data) : this(data.ToArray()) { }

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
            return new Sn<T>(a, Label);
        }

        public override ISeries Clone(string label) => new Sn<T>(Data, label);


        public static implicit operator Sn<T>(T[] array) => new Sn<T>(array);

        public static Sn<double> Rnd(int length, string label = "")
        {
            double[] values = new double[length];
            var rnd = new Random(DateTime.Now.Millisecond);
            for (int i = 0; i < length; i++)
            {
                values[i] = rnd.NextDouble();
            }
            return new Sn<double>(values, label);
        }

        public static Sn<int> Rnd(int length, int max, string label = "")
        {
            int[] values = new int[length];
            var rnd = new Random(DateTime.Now.Millisecond);
            for (int i = 0; i < length; i++)
            {
                values[i] = rnd.Next(max);
            }
            return new Sn<int>(values, label);
        }
    }
}
