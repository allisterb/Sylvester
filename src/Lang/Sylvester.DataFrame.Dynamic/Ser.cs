using System;
using System.Collections.Generic;
using System.Text;

namespace Sylvester
{
    public abstract class Ser<T> : ISeries
    {
        public Ser(T[] data, string label) { Data = data; Label = label; }

        public T[] Data { get; }

        public Type DataType { get; } = typeof(T);

        public Array _Data => Data;

        public string Label { get; }

        public int Length => Data.Length;

        public ref T this[int index] => ref Data[index]; 
    }
}
