using System;
using System.Collections.Generic;
using System.Text;

namespace Sylvester
{
    public class DefaultBackend: Backend
    {
        public DefaultBackend() : base() {}

        public override T[] Fill<T>(T[] a, ref T value)
        {
            for (int i = 0; i < a.Length; i++)
            {
                ref T v = ref a[i];
                v = ref value;
            }
            return a;
        }

        public override T[] FillRef<T>(T[] a, ref T value)
        {
            for (int i = 0; i < a.Length; i++)
            {
                ref T v = ref a[i];
                v = ref value;
            }
            return a;
        }
    }
}
