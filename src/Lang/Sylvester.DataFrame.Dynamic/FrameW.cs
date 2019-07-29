using System;

using System.Collections;
using System.Dynamic;
using System.Runtime.CompilerServices;
using System.Linq;
using System.Text;

using Microsoft.CSharp.RuntimeBinder;

namespace Sylvester
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

        public FrameDR[] SelC(params ISeries[] series) => Frame.SelC(series);

        public FrameDR[] SelC(params string[] series) => Frame.SelC(series);

        public FrameDR[] SelC(params int[] series) => Frame.SelC(series);

        public FrameDR[] ExC(params ISeries[] series) => Frame.ExC(series);

        public FrameDR[] ExC(params string[] series) => Frame.ExC(series);

        public FrameDR[] ExC(params int[] series) => Frame.ExC(series);
    }
}
