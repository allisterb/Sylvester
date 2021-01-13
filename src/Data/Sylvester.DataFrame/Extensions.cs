using System;
using System.Collections.Generic;
using System.Text;

namespace Sylvester.Data
{
    public static class Extensions
    {
        public static Cn<T> ToColumn<T>(this IEnumerable<T> values) where T : struct, IEquatable<T>, IComparable<T>, IConvertible
        {
            return new Cn<T>(values);
        }

        public static Cs ToColumn(this IEnumerable<string> values) => new Cs(values);

        public static Cd ToColumns(this IEnumerable<DateTime> values) => new Cd(values);

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
