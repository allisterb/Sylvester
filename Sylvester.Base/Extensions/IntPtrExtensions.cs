using System;
using System.Diagnostics;

namespace Sylvester
{
    public static class IntPtrExtensions
    {
        [DebuggerStepThrough]
        public static bool IsZero(this IntPtr ptr)
        {
            return (ptr == IntPtr.Zero);
        }

        [DebuggerStepThrough]
        public static bool IsNotZero(this IntPtr ptr)
        {
            return (ptr != IntPtr.Zero);
        }
    }
}