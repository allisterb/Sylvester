using System;
using Sylvester.Compiler.PlaidML.Bindings;

namespace Sylvester.Compiler.PlaidML
{
    public class Gradient : Value
    {
        public Value OutputValue { get; protected set; }


        public Gradient(Context ctx, Value v) : base(ctx)
        {
            _ptr = plaidml.__Internal.PlaidmlAllocGradient(v);
            if (_ptr.IsZero())
            {
                ReportApiCallError("plaidml_alloc_gradient");
                return;
            }

            OutputValue = v;
            IsAllocated = true;
        }

        public Value ComputeWrt(Variable v)
        {
            ThrowIfNotAllocated();

            IntPtr g = plaidml.__Internal.PlaidmlComputeGradWrt(this, v);
            if (g.IsZero())
            {
                ReportApiCallError("plaidml_compute_grad_wrt");
                return null;
            }

            return new Value(this.Context, "GRAD" + v.Name, g);
        }

        public override void Free()
        {
            base.Free();
            plaidml.__Internal.PlaidmlFreeGradient(this);
            _ptr = IntPtr.Zero;
        }
    }
}