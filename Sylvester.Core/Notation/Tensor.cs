using System;
using System.Collections;
using System.Collections.Generic;
using System.Runtime.CompilerServices;
using System.Reflection;
using System.Diagnostics;
using System.Linq;
using System.Linq.Expressions;

using Humanizer;

using Sylvester.Compiler;
using Sylvester.Trees;

namespace Sylvester.Notation
{
    public partial class Tensor : Term, IAlgebra<Tensor, TensorExpression>, ITermShape
    {
        public Shape Shape { get; protected set; }

        public int[] Dimensions => 
            Shape.Count > 0 ? Shape.Select(d => d.Length).ToArray() : new int[0];

        public int[] Strides => 
            Shape.Count > 0 ? Shape.Select(d => d.Stride).ToArray() : new int[0];

        public int Rank => Dimensions.Length;

        public int ElementCount
        {
            get
            {
                var n = 1;
                for (int i = 0; i < Dimensions.Length; i++)
                {
                    n *= Dimensions[i];
                }

                return n;
            }
        }

        public (IndexSet IndexSet, TensorContraction Expression) ContractionDefinition { get; protected set; }

        public TensorExpression  ElementwiseDefinition { get; protected set; }

#pragma warning disable IDE1006
        public TensorExpression def
        {
            get => IsDefined ? IsElementwiseDefined ? ElementwiseDefinition : ContractionDefinition.Expression : null;
            set
            {
                if (value is TensorIndexExpression)
                {
                    ContractionDefinition = (null, new TensorContraction((TensorIndexExpression) value, this, null));
                }
                else
                {
                    ElementwiseDefinition = new TensorExpression(value, this);
                }
            }
        }
#pragma warning restore IDE1006

        public bool IsDefined => ContractionDefinition.Expression != null || ElementwiseDefinition != null;

        public bool IsElementwiseDefined => ElementwiseDefinition != null;

        public bool IsContractionDefined => ContractionDefinition.Expression != null;

        internal override Expression LinqExpression { get; }

        internal override Name DefaultNameBase { get; } = "A";

        protected (Tensor tensor, int index)? GeneratorContext { get; set; }

        protected Tensor(string name) : base(name)
        {
            LinqExpression = Expression.Constant(this);
        }

        public Tensor(string name, params int[] dim) : this(name)
        {
            if (dim == null)
            {
                throw new ArgumentNullException("The dim parameter cannot be null in this parameter.");
            }
            Shape = new Shape(dim, this);
        }

        public Tensor(params int[] dim) : this("A", dim) {}

        public Tensor(string name, string indexNameBase, out IndexSet I, params int[] dim) : this(name, dim)
        {
            I = new IndexSet(this, indexNameBase, dim);
        }

        public Tensor(ITermShape shape) : this(shape.Label, shape.Dimensions) {}

        public Tensor(string name, TensorExpression expr) : this(name)
        {
            def = expr;
            Shape = expr.Shape;
        }


        public TensorIndexExpression this[IndexSet I]
        {
            get
            {
                ThrowIfIndicesExceedRank(1);
                List<Dimension> dim = new List<Dimension>(Shape.Count - I.Indices.Count);
                int[] tdim = new int[I.Indices.Count];
                int[] tidx = new int[I.Indices.Count];
     
                for (int i = 0; i < tdim.Length; i++)
                {
                    tdim[i] = 1;
                }
                
                for (int i = 0; i < this.Dimensions.Length; i++)
                {
                    if (!I.IntegerIndices.Contains(i))
                    {
                        dim.Add(this.Shape[i]);
                    }
                }
                
                Array t = Array.CreateInstance(typeof(Tensor), tdim);
                t.SetValue(this, tidx);
                Expression[] e = I.Indices.Select(i => Expression.Parameter(typeof(int), i.Id)).ToArray();
                return new TensorIndexExpression(Expression.ArrayAccess(Expression.Constant(t), e), I, dim.ToArray());
            }
            set
            {
                ThrowIfAlreadyAssiged();
                if (value is TensorContraction && value.LinqExpression.NodeType ==
                    System.Linq.Expressions.ExpressionType.Call)
                {
                    ContractionDefinition = (I, value as TensorContraction);
                }
                else if (value is TensorIndexExpression)
                {
                    ContractionDefinition = (I, new TensorContraction(Math.Sum(value), this, I));
                }
            }
        }

        public TensorIndexExpression this[params Index[] indices]
        {
            get
            {
                ThrowIfIndicesExceedRank(indices.Length);
                var t = Array.CreateInstance(typeof(Tensor), indices.Select((i,d) => this.Dimensions[d]).ToArray());
                t.SetValue(this, new int[t.Rank]);
                Dimension[] dim = indices.Select((i, d) => i.Type == IndexType.Dimension ? this.Shape[d] : 0).ToArray();
                return new TensorIndexExpression(Expression.ArrayAccess(Expression.Constant(t), 
                    indices.Select(i => Expression.Parameter(typeof(int), i.Id)).ToArray()), new IndexSet(this, indices),
                    dim);
            }
            set
            {
                ThrowIfAlreadyAssiged();
                IndexSet s = new IndexSet(this);
                Dimension[] dim = indices.Select((i, d) => i.Type == IndexType.Dimension? this.Shape[d] : 0).ToArray();
                TensorContraction tc = new TensorContraction(value, this, s, dim);
                ContractionDefinition = (s, tc);
            }
        }

        public TensorIndexExpression this[Index index, Dimension N]
        {
            get
            {
                ThrowIfIndicesExceedRank(2);
                IndexSet s = new IndexSet(this, index);
                return new TensorIndexExpression(Expression.ArrayAccess(Expression.Constant(new Tensor[] {this}), 
                    new Expression[] { Expression.Parameter(typeof(int), index.Id) }), s, new[] { N });
            }
            set
            {
                ThrowIfAlreadyAssiged();
                Dimension[] shape = new[] { N };
                IndexSet s = new IndexSet(this, index);
                TensorContraction tc = new TensorContraction(value, this, s, shape);
                ContractionDefinition = (s, tc);
            }
        }

        public Dimension this[int dimension]
        {
            get
            {
                return this.Shape.ElementAt(dimension);
            }
        }

        public static implicit operator TensorExpression(Tensor t)
        {
            if (t.IsElementwiseDefined)
            {
                return t.ElementwiseDefinition;
            }
            else
            {
                return new TensorExpression(Expression.Constant(t), t.Shape);
            }
        }

        public static explicit operator TensorIndexExpression(Tensor t)
        {
            if (t.IsContractionDefined)
            {
                return t.ContractionDefinition.Expression;
            }
            else
            {
                return t[(IndexSet)t.Shape];
            }
        }

        public static implicit operator ExpressionTree(Tensor t)
        {
            if (t.IsContractionDefined)
            {
                return t.ContractionDefinition.Expression.ToTree();
            }
            else if (t.IsElementwiseDefined)
            {
                return t.ElementwiseDefinition.ToTree();
            }
            else
            {
                return new TensorExpression(t.LinqExpression).ToTree();
            }
        }

        public static TensorExpression operator -(Tensor left) => left.Negate();

        public static TensorExpression operator +(Tensor left, Tensor right) => left.Add(right);

        public static TensorExpression operator -(Tensor left, Tensor right) => left.Subtract(right);

        public static TensorExpression operator *(Tensor left, Tensor right) => left.Multiply(right);

        public static TensorExpression operator /(Tensor left, Tensor right) => left.Divide(right);


        public virtual TensorExpression Negate() => -(TensorExpression) this;

        public virtual TensorExpression Add(Tensor right) => (TensorExpression) this + right;

        public virtual TensorExpression Subtract(Tensor right) => (TensorExpression) this - right;

        public virtual TensorExpression Multiply(Tensor right) => (TensorExpression) this * right;

        public virtual TensorExpression Divide(Tensor right) => (TensorExpression) this / right;


        public Tensor With(out Tensor with)
        {
            GeneratorContext = GeneratorContext ?? ((this, 1));
            with = new Tensor(GenerateName(GeneratorContext.Value.index, Name), Dimensions);
            GeneratorContext = (GeneratorContext.Value.tensor, GeneratorContext.Value.index + 1);
            return GeneratorContext.Value.tensor;
        }

        public Tensor With(out Tensor with, string name)
        {
            GeneratorContext = GeneratorContext ?? (this, 1);
            with = new Tensor(name, Dimensions);
            GeneratorContext = (GeneratorContext.Value.tensor, GeneratorContext.Value.index + 1);
            return GeneratorContext.Value.tensor;
        }

        public Tensor With(out Tensor with, params int[] dim)
        {
            GeneratorContext = GeneratorContext ?? (this, 1);
            if (dim.Length != GeneratorContext.Value.tensor.Dimensions.Length)
            {
                throw new ArgumentException($"The rank of the new tensor must be the same as the original: " +
                                            $"{dim.Length}.");
            }

            with = new Tensor(GenerateName(GeneratorContext.Value.index, Name), dim);
            GeneratorContext = (GeneratorContext.Value.tensor, GeneratorContext.Value.index + 1);
            return GeneratorContext.Value.tensor;
        }

        public Tensor With(out Tensor with, string name, params int[] dim)
        {
            GeneratorContext = GeneratorContext ?? (this, 1);
            if (dim.Length != GeneratorContext.Value.tensor.Dimensions.Length)
            {
                throw new ArgumentException($"The rank of the new tensor must be the same as the original: " +
                                            $"{dim.Length}.");
            }

            with = new Tensor(name, dim);
            GeneratorContext = (GeneratorContext.Value.tensor, GeneratorContext.Value.index + 1);
            return GeneratorContext.Value.tensor;
        }

        public ITermShape CloneShape(string name) => new Tensor(name, this.Dimensions);

        public ExpressionTree ToTree()
        {
            if (IsDefined)
            {
                if (IsContractionDefined)
                {
                    return this.ContractionDefinition.Expression.ToTree();
                }
                else
                {
                    return ElementwiseDefinition.ToTree();
                }
            }
            else
            {
                return ((TensorExpression) this).ToTree();
            }
        }

        public Var<T> Var<T>(Array array) where T : unmanaged, IEquatable<T>, IComparable<T>, IConvertible
            => new Var<T>(this, array);

        public Var<T> Var<T>(params T[] data) where T : unmanaged, IEquatable<T>, IComparable<T>, IConvertible
            => new Var<T>(this, data);

        public Var<T> Var<T>() where T : unmanaged, IEquatable<T>, IComparable<T>, IConvertible
            => new Var<T>(this, Array.CreateInstance(typeof(T), Dimensions));

        /// <summary> Only intended for the C# templating process. </summary>
        public static string RankToTensorName(int rank)
        {
            var names = rank.ToWords().Split('-');
            for (int i = 0; i < names.Length; i++)
            {
                names[i] = char.ToUpper(names[i][0]) + names[i].Substring(1);
            }

            return string.Join("", names);
        }

        public IndexSet AxesToIndices(params int[] axes)
        {
            if (axes.Length == 0)
            {
                axes = new[] {Rank - 1};
            }
            else if (axes.Length > Dimensions.Length)
            {
                throw new ArgumentException("The number of axes specified exceeds the rank of this tensor.");
            }
            else if (axes.Length == 1 && axes[0] < 0)
            {
                axes[0] = Rank + axes[0];
            }
            else
            {
                for (int i = 0; i < axes.Length; i++)
                {
                    if (axes[i] < 0)
                    {
                        axes[i] = axes.Length + axes[i];
                    }
                }
            }

            axes = axes.OrderBy(a => a).ToArray();
            string nb = Label.ToLower();
            Index[] indices = Dimensions.Select((o, d) => new Index(null, o, d, nb + o.ToString())).ToArray();
            IndexSet S = new IndexSet(this, indices);
            return S;
        }

        public IEnumerator<int> GetEnumerator()
        {
            for (int i = 0; i < Dimensions.Length; i++)
            {
                yield return Dimensions[i];
            }
        }

        IEnumerator IEnumerable.GetEnumerator() => GetEnumerator();

        [DebuggerStepThrough]
        internal void ThrowIfAlreadyAssiged()
        {
            if (IsDefined)
            {
                throw new InvalidOperationException(
                    $"This tensor variable has an existing assigment. You can only assign to a tensor variable once.");
            }
        }

        [DebuggerStepThrough]
        internal void ThrowIfIndicesExceedRank(int c)
        {
            if (Rank < c)
                throw new ArgumentOutOfRangeException(nameof(c),
                    "The number of indices exceeds the number of dimensions of this tensor.");
        }

        [DebuggerStepThrough]
        internal void ThrowIfIndicesNotEqualToRank(int c)
        {
            if (Rank != c)
                throw new ArgumentOutOfRangeException(nameof(c),
                    "The number of indices is not the same as the number of dimensions of this tensor.");
        }

        [DebuggerStepThrough]
        internal void ThrowIfAxisExceedRank(int a)
        {
            if (Rank < a)
                throw new ArgumentOutOfRangeException(nameof(a), "The specified axis exceeds the rank of this tensor.");
        }
    }
}