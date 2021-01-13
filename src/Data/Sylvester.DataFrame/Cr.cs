﻿using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Sylvester.Data
{
    public class Cr<T> : FrameC<T>, IEnumerable<T> where T : class, IEquatable<T>, IComparable<T>
    {
        public Cr(T[] data, string label, object defaultVal = default) : base(label, defaultVal)
        {
            Data = data;
        }

        public Cr(T[] data) : this(data, "") {}

        public Cr(IEnumerable<T> data) : this(data.ToArray()) {}

        public T[] Data { get; }

        public override int Length => Data.Length;

        public override ref T Ref(int index) => ref Data[index];

        public override IEnumerator GetEnumerator() => Data.GetEnumerator();

        IEnumerator<T> IEnumerable<T>.GetEnumerator() => (Data as IEnumerable<T>).GetEnumerator();

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
        public override IColumn Append(params dynamic[] values)
        {
            T[] a = new T[Data.Length + values.Length];
            Data.CopyTo(a, 0);
            values.CopyTo(a, Length);
            return new Cr<T>(a, Label);
        }

        public override IColumn Clone(string label) => new Cr<T>(Data, label);


    }
}
