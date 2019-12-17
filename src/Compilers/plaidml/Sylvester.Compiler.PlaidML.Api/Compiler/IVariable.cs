using System;
using System.Buffers;
using System.Collections.Generic;

namespace Sylvester.Compiler
{
    public interface IVariable<T> : IEnumerable<T>, IShape, INDArray, IPinnable
        where T : unmanaged, IEquatable<T>, IComparable<T>, IConvertible
    {
        bool Initialized { get; }

        MemoryHandle MemoryHandle { get; }

        ref T Read(int index);

        void Write(int index, T value);

        ref T this[int index] { get; }

        Span<T> Span { get; }
    }
}