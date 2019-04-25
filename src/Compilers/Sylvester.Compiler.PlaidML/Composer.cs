using System;
using System.Collections.Generic;
using System.Linq;
using Sylvester.Compiler.PlaidML.Bindings;

namespace Sylvester.Compiler.PlaidML
{
    public class Composer : PlaidMLApi<Composer>
    {
        public List<Value> Inputs { get; protected set; }

        public List<Value> Outputs { get; protected set; }

        public List<Applier> Dependencies { get; protected set; }

        public Dictionary<Value, Value> Updates { get; protected set; }

        public Composer(Context ctx) : base(ctx)
        {
            _ptr = plaidml.__Internal.PlaidmlAllocComposer();

            if (_ptr.IsZero())
            {
                ReportApiCallError("plaidml_alloc_composer");
                throw new PlaidMLApiException<Composer>(this, "Could not allocate composer.");
            }
            else
            {
                IsAllocated = true;
                Inputs = new List<Value>();
                Outputs = new List<Value>();
                Dependencies = new List<Applier>();
                Updates = new Dictionary<Value, Value>();
            }
        }

        public bool AddInputPlaceholder(string name, ulong dimensionCount)
        {
            ThrowIfNotAllocated();
            var p = new Placeholder(this._context, name, dimensionCount);
            if (p.IsAllocated)
            {
                var r = plaidml.__Internal.PlaidmlAddComposerInput(this, p.Name, p);
                if (r)
                {
                    Inputs.Add(p);
                }

                return r;
            }
            else
            {
                return false;
            }
        }

        public bool AddOutputValue(Value o)
        {
            ThrowIfNotAllocated();

            if (!o.IsAllocated)
            {
                return false;
            }

            var r = plaidml.__Internal.PlaidmlAddComposerOutput(this, o.Name, o);
            if (r)
            {
                Outputs.Add(o);
            }

            return r;
        }

        public bool AddOutputValue(string name, IntPtr varPtr)
        {
            ThrowIfNotAllocated();
            var v = new Value(_context, name, varPtr);

            if (v.IsAllocated)
            {
                return AddOutputValue(v);
            }
            else
            {
                return false;
            }
        }

        public bool AddDependency(Applier applier)
        {
            ThrowIfNotAllocated();

            if (!applier.IsAllocated)
            {
                throw new ArgumentException("The Applier is not allocated.");
            }

            var r = plaidml.__Internal.PlaidmlAddComposerDependency(this, applier);
            if (r)
            {
                Dependencies.Add(applier);
            }
            else
            {
                ReportApiCallError("plaidml_add_composer_dependency");
            }

            return r;
        }

        public bool AddUpdate(Value destination, Value src)
        {
            ThrowIfNotAllocated();

            if (!src.IsAllocated)
            {
                throw new ArgumentException("The src Value is not allocated.");
            }

            if (!destination.IsAllocated)
            {
                throw new ArgumentException("The destination Value is not allocated.");
            }

            if (!Outputs.Select(v => v.Name).Contains(destination.Name))
            {
                throw new ArgumentException("The destination Value is not an output of this composer.");
            }

            var r = plaidml.__Internal.PlaidmlAddComposerUpdate(this, destination, src);
            if (r)
            {
                Updates.Add(src, destination);
            }
            else
            {
                ReportApiCallError("plaidml_add_composer_update");
            }

            return r;
        }

        public Function BuildFunction()
        {
            ThrowIfNotAllocated();

            if (Inputs.Count == 0)
            {
                throw new InvalidOperationException("There are no inputs defined for the composer.");
            }

            if (Outputs.Count == 0)
            {
                throw new InvalidOperationException("There are no outputs defined for the composer.");
            }

            var p = plaidml.__Internal.PlaidmlBuildComposedFunction(this);
            if (p.IsZero())
            {
                ReportApiCallError("plaidml_build_composed_function");
                throw new PlaidMLApiException<Composer>(this, "Could not build composed function.");
            }

            return new Function(_context, p);
        }

        public override void Free()
        {
            base.Free();
            plaidml.__Internal.PlaidmlFreeComposer(this);
            _ptr = IntPtr.Zero;
        }
    }
}