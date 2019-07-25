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
        public FrameC(string label, dynamic defaultVal = null) { Label = label; DefaultVal = defaultVal; }

        public FrameC(string label) : this(label, default) {}

        public FrameC() : this("") {}

        public Type DataType { get; } = typeof(T);

        public string Label { get; }

        public abstract int Length { get; }

        public abstract IEnumerator GetEnumerator();

        public abstract ref T Ref(int index);

        public abstract T this[int index] { get; }

        public dynamic DefaultVal { get; protected set; }

        public dynamic GetVal(int index) => this[index];

        public abstract bool SetVal(int index, dynamic value);

        public IBackend Backend { get; set; }

        public abstract ISeries Append(params dynamic[] values);

        public abstract ISeries Clone(string label);
    }

}
