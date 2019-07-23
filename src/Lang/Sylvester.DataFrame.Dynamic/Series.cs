using System;
using System.Dynamic;
using System.Reflection;
using System.Collections.Generic;
using System.Text;
using System.Linq;
using System.Linq.Expressions;

namespace Sylvester
{
    public abstract class Series<T> : DynamicObject, ISeries
    {
        public Series(T[] data, string label) { Data = data; Label = label; }

        public Type DataType { get; } = typeof(T);

        public string Label { get; }

        public int Length => Data.Length;

        public T[] Data { get; }

        public T this[int index] => Data[index];

        public ref T Ref(int index) => ref Data[index];
 
    }

}
