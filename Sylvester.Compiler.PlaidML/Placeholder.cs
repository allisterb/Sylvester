using Sylvester.Compiler.PlaidML.Bindings;

namespace Sylvester.Compiler.PlaidML
{
    public class Placeholder : Value
    {
        public ulong DimensionCount { get; protected set; }

        public Placeholder(Context ctx, string name, ulong dimensionCount) : base(ctx, name)
        {
            _ptr = plaidml.__Internal.PlaidmlAllocPlaceholder(dimensionCount);
            if (_ptr.IsZero())
            {
                ReportApiCallError("plaidml_alloc_placeholder");
            }
            else
            {
                IsAllocated = true;
                DimensionCount = dimensionCount;
            }
        }
    }
}