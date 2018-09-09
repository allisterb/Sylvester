using System;
using Sylvester.Compiler.PlaidML.Bindings;

namespace Sylvester.Compiler.PlaidML
{
    public class Invocation<T> : PlaidMLApi<Invocation<T>>
        where T : unmanaged, IEquatable<T>, IComparable<T>, IConvertible
    {
        public Invoker<T> Invoker { get; protected set; }

        public Invocation(Context ctx, Invoker<T> invoker) : base(ctx)
        {
            _ptr = plaidml.__Internal.PlaidmlScheduleInvocation(_context, invoker);
            if (_ptr.IsZero())
            {
                ReportApiCallError("plaidml_schedule_invocation");
                return;
            }

            Invoker = invoker;
            IsAllocated = true;
        }

        public override void Free()
        {
            base.Free();
            plaidml.__Internal.PlaidmlFreeInvocation(this);
            _ptr = IntPtr.Zero;
        }
    }
}