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

        public FrameDR[] Ser(params ISeries[] series) => Frame.Ser(series);

        public FrameDR[] Ser(params string[] series) => Frame.Ser(series);

        public FrameDR[] Ser(params int[] series) => Frame.Ser(series);

        public FrameDR[] Ex(params ISeries[] series) => Frame.Ex(series);

        public FrameDR[] Ex(params string[] series) => Frame.Ex(series);

        public FrameDR[] Ex(params int[] series) => Frame.Ex(series);
    }
}
