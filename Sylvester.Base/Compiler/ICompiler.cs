using System;
using System.Collections.Generic;

namespace Sylvester.Compiler
{
    public interface ICompiler
    {
        Dictionary<string, object> Options { get; }

        ITensorContext TensorContext { get; }

        CompilerStatus Status { get; }

        string CompilerStatusMessage { get; }

        bool Initialized { get; }

        bool Compile<T>(IKernel<T> kernel, out IRunnable<T> result) where T : unmanaged, IEquatable<T>,
            IComparable<T>, IConvertible;
    }
}