using System;
using System.Dynamic;
using System.Reflection;
using System.Collections;
using System.Text;
using System.Linq;
using System.Linq.Expressions;

namespace Sylvester
{
    public abstract class FrameC<T> : DynamicObject, ISeries where T : IEquatable<T>
    {
        public FrameC(string label) { Label = label; }

        public Type DataType { get; } = typeof(T);

        public string Label { get; }

        public abstract int Length { get; }

        public abstract IEnumerator GetEnumerator();

        public abstract ref T Ref(int index);

        public abstract T this[int index] { get; }

        public dynamic GetVal(int index) => this[index];

        public abstract bool SetVal(int index, dynamic value);

        public abstract ISeries Append(params dynamic[] values);

    }

}
