using System;

using System.Collections;
using System.Dynamic;
using System.Runtime.CompilerServices;
using System.Linq;
using System.Text;

using Microsoft.CSharp.RuntimeBinder;

namespace Sylvester.Data
{
    public class FrameW<T> : IEnumerable where T : IEquatable<T>
    {
        public FrameW(Frame f, Func<T, int> index)
        {
            Frame = f;
            Index = index;
        }

        public Frame Frame { get; }

        public Func<T, int> Index { get; }

        public FrameR this[T t] => Frame[Index(t)];

        public IEnumerator GetEnumerator() => Frame.GetEnumerator();

        public FrameV<int> Col(params IColumn[] columns) => Frame.ColsV(columns);

        public FrameV<int> Col(params string[] columns) => Frame.ColsV(columns);

        public FrameV<int> Col(params int[] columns) => Frame.ColsV(columns);

        public FrameV<int> Ex(params IColumn[] columns) => Frame.ExV(columns);

        public FrameV<int> Ex(params string[] columns) => Frame.ExV(columns);

        public FrameV<int> Ex(params int[] columns) => Frame.ExV(columns);
    }
}
