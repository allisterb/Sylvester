using System;
using Sylvester.Compiler.PlaidML.Bindings;

namespace Sylvester.Compiler.PlaidML
{
    public class Variable : PlaidMLApi<Variable>
    {
        public string Name { get; protected set; }

        public IntPtr VarPtr => this._ptr;

        public PlaidmlDatatype DataType { get; protected set; }


        protected Variable(Context ctx) : base(ctx)
        {
        }

        protected Variable(Context ctx, string name) : base(ctx)
        {
            Name = name;
        }

        public Variable(Context ctx, string name, Int64 v) : this(ctx, name)
        {
            _ptr = plaidml.__Internal.PlaidmlAllocInt64(v);
            if (_ptr.IsZero())
            {
                ReportApiCallError("plaidml_alloc_int64");
                return;
            }

            DataType = PlaidmlDatatype.PLAIDML_DATA_INT64;
            IsAllocated = true;
        }

        public Variable(Context ctx, string name, Int32 v) : base(ctx)
        {
            _ptr = plaidml.__Internal.PlaidmlAllocInt64(v);
            if (_ptr.IsZero())
            {
                ReportApiCallError("plaidml_alloc_int64");
                return;
            }

            DataType = PlaidmlDatatype.PLAIDML_DATA_INT64;
            IsAllocated = true;
        }

        public Variable(Context ctx, string name, double v) : base(ctx)
        {
            _ptr = plaidml.__Internal.PlaidmlAllocReal(v);
            if (_ptr.IsZero())
            {
                ReportApiCallError("plaidml_alloc_real");
                return;
            }

            Name = name;
            DataType = PlaidmlDatatype.PLAIDML_DATA_FLOAT64;
            IsAllocated = true;
        }

        public Variable(Context ctx, string name, float v) : base(ctx)
        {
            _ptr = plaidml.__Internal.PlaidmlAllocReal(v);
            if (_ptr.IsZero())
            {
                ReportApiCallError("plaidml_alloc_real");
                return;
            }

            Name = name;
            DataType = PlaidmlDatatype.PLAIDML_DATA_FLOAT64;
            IsAllocated = true;
        }


        public override void Free()
        {
            base.Free();
            plaidml.__Internal.PlaidmlFreeVar(_ptr);
            _ptr = IntPtr.Zero;
        }
    }
}