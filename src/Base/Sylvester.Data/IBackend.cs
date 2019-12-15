using System;

namespace Sylvester.Data
{
    public interface IBackend
    {
        T[] FillRef<T>(T[] a, ref T value);
        T[] Fill<T>(T[] a, ref T value) where T : struct;
    }
}
