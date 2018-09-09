using System;
using System.Collections.Generic;
using System.Text;

namespace Sylvester.Compiler
{
    public class RunException<T> : Exception where T : unmanaged, IEquatable<T>, IComparable<T>, IConvertible
    {
        public Kernel<T> Kernel { get; protected set; }
        public IRunnable<T> Runnable { get; protected set; }
        public RunStatus Status { get; protected set; }
        public RunException(Kernel<T> kernel, IRunnable<T> runnable, RunStatus status, string message) : base(message)
        {
            Kernel = kernel;
            Runnable = runnable;
            Status = status;
        }

    }
}
