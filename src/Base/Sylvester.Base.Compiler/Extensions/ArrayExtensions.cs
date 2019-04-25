using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;

namespace Sylvester
{
    public static class ArrayExtensions
    {
        [DebuggerStepThrough]
        public static IEnumerable<T> Flatten<T>(this Array array)
        {
            return array.Cast<T>();
        }
    }
}