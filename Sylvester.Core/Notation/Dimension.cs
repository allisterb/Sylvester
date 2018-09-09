using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using System.Reflection;
using System.Text;

namespace Sylvester.Notation
{
    public class Dimension : Term, IChildTerm, IAlgebra<Dimension, Dimension>
    {
        public Tensor Tensor { get; internal set; }

        public ITerm Parent => Tensor;

        public DimensionType DimensionType { get; protected set; }

        public int Order { get; protected set; }

        public int Length { get; protected set; }

        public int Stride { get; protected set; }

        protected Expression DimensionExpression { get; set; }

        internal override Name DefaultNameBase { get; } = "N0";

        internal override Expression LinqExpression => DimensionExpression;

        protected Dimension(Tensor t, int order, string label) : base(label)
        {
            this.Tensor = t;
            this.Order = order;
        }

        internal Dimension(Tensor t, int order, int length, int stride) : this(t, order, t.Label + "DIM" + order.ToString())
        {
            this.Length = length;
            this.Stride = stride;
            this.DimensionExpression = Expression.Constant(Length);
            this.DimensionType = DimensionType.Constant;
        }

        internal Dimension(Expression dimExpression) : this(null, -1, 
            "dim_" + GetNameFromLinqExpression(dimExpression))
        {
            this.DimensionExpression = dimExpression;
            this.DimensionType = DimensionType.Expression;
        }

        internal Dimension(int d) : this(Expression.Constant(d)) {}

        internal Dimension(int d, int order) : this(Expression.Constant(d))
        {
            Order = order;
        }

        public static Dimension operator -(Dimension left) => left.Negate();

        public static Dimension operator +(Dimension left, Dimension right) => left.Add(right);

        public static Dimension operator -(Dimension left, Dimension right) => left.Subtract(right);

        public static Dimension operator *(Dimension left, Dimension right) => left.Multiply(right);

        public static Dimension operator /(Dimension left, Dimension right) => left.Divide(right);

        public static implicit operator Dimension(int d) => new Dimension(d);

        public static explicit operator Scalar(Dimension d) => d.DimensionType == DimensionType.Constant ?
            new Scalar(d.Label, true) : throw new ArgumentException("The specified dimension is not a dimension constant");

        public Dimension Negate() => new Dimension(Expression.Negate(DimensionExpression, 
            GetDummyUnaryMethodInfo<Dimension, Dimension>(this)));

        public Dimension Add(Dimension right) => new Dimension(Expression.Add(this, 
            right.DimensionExpression, GetDummyBinaryMethodInfo<Dimension, Dimension>(this, right)));

        public Dimension Subtract(Dimension right) => new Dimension(Expression.Subtract(this, 
            right.DimensionExpression, GetDummyBinaryMethodInfo<Dimension, Dimension>(this, right)));

        public Dimension Multiply(Dimension right) => new Dimension(Expression.Multiply(this, 
            right.DimensionExpression, GetDummyBinaryMethodInfo<Dimension, Dimension>(this, right)));

        public Dimension Divide(Dimension right) => new Dimension(Expression.Divide(this, 
            right.DimensionExpression, GetDummyBinaryMethodInfo<Dimension, Dimension>(this, right)));

        public static Dimension[] ToArray((Dimension n1, Dimension n2) dim) => new [] {dim.n1, dim.n2};
        public static Dimension[] ToArray((Dimension n1, Dimension n2, Dimension n3) dim) => new[] { dim.n1, dim.n2 , dim.n3};



        private static Dimension DummyUnary(Dimension l) => null;
        private static Dimension DummyBinary(Dimension l, Dimension r) => null;
    }

}