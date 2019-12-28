using System;
using System.Diagnostics;

namespace Sylvester
{
    public static class StringExtensions
    {
        [DebuggerStepThrough]
        public static bool IsNullOrEmpty(this string s)
        {
            return string.IsNullOrEmpty(s);
        }

        [DebuggerStepThrough]
        public static bool IsNotNullOrEmpty(this string s)
        {
            return !string.IsNullOrEmpty(s);
        }

        [DebuggerStepThrough]
        public static void ThrowIfEmpty(this string s, string message)
        {
            if (string.IsNullOrEmpty(s))
            {
                throw new InvalidOperationException(message);
            }
        }
    }
}