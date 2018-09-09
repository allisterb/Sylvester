using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Runtime.CompilerServices;

namespace Sylvester.Notation
{
    public class Shape : List<Dimension>
    {
        public Tensor Tensor { get; protected set; }

        internal Shape() : base() {}

        internal Shape(params Dimension[] dim) : base(dim)
        {
            if (dim == null)
            {
                throw new ArgumentNullException("The dim parameter cannot be null in this constructor.");
            }
        }

        internal Shape(Tensor t, params Dimension[] dim) : base(dim)
        {
            Tensor = t;
            if (dim == null)
            {
                throw new ArgumentNullException("The dim parameter cannot be null in this constructor.");
            }
        }


        internal Shape(int[] dim, Tensor t) 
        {
            if (dim == null)
            {
                throw new ArgumentNullException("The dim parameter cannot be null in this constructor.");
            }
            int[] strides = StridesInElements(dim);
            for (int i = 0; i < dim.Length; i++)
            {
                base.Add(new Dimension(t, i, dim[i], strides[i]));
            }
            this.Tensor = t;
        }

        public Dimension this[Index i]
        {
            get
            {
                if(i.Type != IndexType.Dimension)
                {
                    throw new ArgumentException("This index is a dimension expression or literal, not a dimension.");
                }
                else
                {
                    Tensor?.ThrowIfIndicesExceedRank(i.Order);
                    return base[i.Order];
                }
            }
        }

        public new Dimension this[int i] => base[i];

        public static explicit operator IndexSet(Shape d) => 
            new IndexSet(d.Tensor ?? throw new InvalidCastException("No tensor is associated with this Shape."));

        public List<Index> ToIndices() => this.Select((d, i) => new Index(i)).ToList();
        
        public static int[] StridesInElements(int[] dim)
        {
            if (dim == null)
            {
                throw new ArgumentNullException("The dim parameter cannot be null.");
            }
            if (dim.Length == 0)
            {
                return new int[0];
            }
            var strides = new int[dim.Length];
            float s = 1;
            for (int i = 0; i < dim.Length; i++)
            {
                if (dim[i] > 0)
                {
                    s *= Convert.ToSingle(dim[i]);
                }
            }

            for (int i = 0; i < dim.Length; i++)
            {
                if (dim[i] > 0)
                {
                    s /= Convert.ToSingle(dim[i]);
                    strides[i] = Convert.ToInt32(s);
                }
            }

            return strides;
        }

        public static int[] StridesInBytes<T>(int[] dim)
        {
            var strides = StridesInElements(dim);
            for (int i = 0; i < strides.Length; i++)
            {
                strides[i] *= Unsafe.SizeOf<T>();
            }

            return strides;
        }
    }
}
