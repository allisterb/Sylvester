using System;

using System.Collections;
using System.Collections.Generic;
using System.Dynamic;
using System.Runtime.CompilerServices;
using System.Linq;
using System.Text;

using Microsoft.CSharp.RuntimeBinder;

namespace Sylvester.Data
{
    public class FrameV<T> : IEnumerable where T : IEquatable<T>
    {
        public FrameV(IEnumerable<FrameDR> rows, Func<T, FrameV<T>, int> index)
        {
            Rows = rows.ToList();
            Index = index;
        }

        public FrameV(List<FrameDR> rows, Func<T, FrameV<T>, int> index)
        {
            Rows = rows;
            Index = index;
        }

        public List<FrameDR> Rows { get; }

        public Func<T, FrameV<T>, int> Index { get; }

        public FrameDR this[T t] => Rows[Index(t, this)];

        public IEnumerator GetEnumerator() => (IEnumerator) Rows.GetEnumerator();

        public FrameV<T> Col(params IColumn[] columns) => new FrameV<T>(Rows.Select(r => r.Col(columns)), Index);

        public FrameV<T> Col(params string[] columns) => new FrameV<T>(Rows.Select(r => r.Col(columns)), Index);

        public FrameV<T> Col(params int[] columns) => new FrameV<T>(Rows.Select(r => r.Col(columns)), Index);

        public FrameV<T> Ex(params IColumn[] columns) => new FrameV<T>(Rows.Select(r => r.Ex(columns)), Index);

        public FrameV<T> Ex(params string[] columns) => new FrameV<T>(Rows.Select(r => r.Ex(columns)), Index);


        public static implicit operator List<FrameDR>(FrameV<T> view) => view.Rows;
    }
}
