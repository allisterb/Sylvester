using System;
using System.Collections.Generic;
using Sylvester.Trees;

namespace Sylvester.Compiler
{
    public interface IKernel<T> where T : unmanaged, IEquatable<T>, IComparable<T>, IConvertible
    {
        DeviceType DeviceType { get; }

        IExpressionTree ExpressionTree { get; }

        IReadOnlyList<ITermShape> InputShapes { get; }

        ITermShape OutputShape { get; set; }

        bool CompileBeforeRun { get; set; }

        ICompiler Compiler { get; }

        bool CompileSuccess { get; }

        IRunnable<T> CompilerResult { get; }

        bool Compile();

        IRunnable<T> Compile(out CompilerStatus status);
    }
}