using System;
using System.Collections.Generic;
using System.Text;

namespace Sylvester.Data
{
    public static class Extensions
    {
        public static Sn<T> ToSeries<T>(this IEnumerable<T> values) where T : struct, IEquatable<T>, IComparable<T>, IConvertible
        {
            return new Sn<T>(values);
        }

        public static Ss ToSeries(this IEnumerable<string> values) => new Ss(values);

        public static Sd ToSeries(this IEnumerable<DateTime> values) => new Sd(values);

        public static FrameV<int> ToFrameV (this IEnumerable<FrameDR> rows)
        {
            return new FrameV<int>(rows, (index, view) => index);
        }

        public static FrameV<T> ToFrameV<T>(this IEnumerable<FrameDR> rows, Func<T, FrameV<T>, int> index) where T : IEquatable<T>
        {
            return new FrameV<T>(rows, index);
        }
    }
}
