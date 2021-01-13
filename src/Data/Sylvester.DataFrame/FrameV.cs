using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;

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

        public FrameV(Frame frame, Func<T, FrameV<T>, int> index)
        {
            Rows.AddRange(frame.Select((r, i) => new FrameDR(frame, i, frame.Columns.ToArray())));
            Index = index;
        }

        public List<FrameDR> Rows { get; }

        public int Length => Rows.Count;

        public Func<T, FrameV<T>, int> Index { get; }

        public FrameDR this[T t] => Rows[Index(t, this)];

        public IEnumerator GetEnumerator() => Rows.GetEnumerator();

        public FrameV<T> Sel(params IColumn[] columns) => new FrameV<T>(Rows.Select(r => r.Sel(columns)), Index);

        public FrameV<T> Sel(params string[] columns) => new FrameV<T>(Rows.Select(r => r.Sel(columns)), Index);

        public FrameV<T> Sel(params int[] columns) => new FrameV<T>(Rows.Select(r => r.Sel(columns)), Index);

        public FrameV<T> Ex(params IColumn[] columns) => new FrameV<T>(Rows.Select(r => r.Ex(columns)), Index);

        public FrameV<T> Ex(params string[] columns) => new FrameV<T>(Rows.Select(r => r.Ex(columns)), Index);

        public static implicit operator List<FrameDR>(FrameV<T> view) => view.Rows;
    }
}
