using System;
using Sylvester.Compiler.PlaidML.Bindings;

namespace Sylvester.Compiler.PlaidML
{
    public class Value : PlaidMLApi<Value>
    {
        public string Name { get; protected set; }

        protected Value(Context ctx) : base(ctx)
        {
        }

        protected Value(Context ctx, string name) : this(ctx)
        {
            Name = name;
        }

        public Value(Context ctx, string name, IntPtr varPtr) : base(ctx)
        {
            if (varPtr.IsZero())
            {
                throw new ArgumentNullException("varPtr");
            }

            _ptr = varPtr;
            Name = name;
            IsAllocated = true;
        }

        public Value(Variable variable) : base(variable.Context)
        {
            _ptr = variable.VarPtr;
            Name = variable.Name;
            IsAllocated = true;
        }

        public static implicit operator Value(Variable v) => new Value(v);

        public override void Free()
        {
            base.Free();
            plaidml.__Internal.PlaidmlFreeVar(_ptr);
            _ptr = IntPtr.Zero;
        }
    }
}