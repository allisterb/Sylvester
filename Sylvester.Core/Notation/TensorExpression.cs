using System;
using System.Collections.Generic;
using System.Reflection;
using System.Linq;
using System.Linq.Expressions;
using Sylvester.Compiler;
using Sylvester.Expressions;
using Sylvester.Trees;

namespace Sylvester.Notation
{
    public partial class TensorExpression : Term, IAlgebra<TensorExpression, TensorExpression>, IShape
    {
        internal override Expression LinqExpression { get; }

        internal override Name DefaultNameBase => "tensor_expr0";

        public Tensor LHSTensor { get; protected set; }

        public IndexSet LHSIndexSet { get; protected set; }

        public List<Tensor> TensorReferences => LinqExpression.GetConstants<Tensor>();

        public Shape Shape { get; protected set; }

        public int[] Dimensions => Shape.Count > 0 ? Shape.Select(d => d.Length).DefaultIfEmpty(0).ToArray() : new int[0];

        public int[] Strides => Shape.Count > 0 ? Shape.Select(d => d.Stride).DefaultIfEmpty(0).ToArray() : new int[0];

        public int Rank => Dimensions.Length;

        public List<Tensor> InputVariables => TensorReferences.Where(t => !t.IsDefined).ToList();

        public List<Tensor> TensorInputVariables { get; protected set; }

        public List<Index> IndexParameters => LinqExpression.GetParameters<Index>();

        public bool IsTensorVariable => LinqExpression.IsTensorVariable();

        internal override Type ExpressionType => LinqExpression.Type;
        
        
        public TensorExpression(Expression e) : base(GetNameFromLinqExpression(e))
        {
            LinqExpression = e;
            Shape = new Shape();
        }

        public TensorExpression(Expression e, Shape shape) : this(e)
        {
            Shape = shape;
        }

        public TensorExpression(Expression e, params Dimension[] dim) : this(e)
        {
            Shape = new Shape(dim);
        }

        public TensorExpression(TensorExpression expr, Tensor lhs) : this(expr.LinqExpression, expr.Shape)
        {
            LHSTensor = lhs; 
        }

        public TensorExpression(Expression e, Tensor lhs, Shape shape) : this(e, lhs)
        {
            Shape = shape;
        }

        public TensorExpression(Expression e, Tensor lhs, params Dimension[] dim) : this(e, dim)
        {
            this.LHSTensor = lhs;
        }


        public Dimension this[int dimension]
        {
            get
            {
                return this.Shape.ElementAt(dimension);
            }
        }

        public static TensorExpression operator -(TensorExpression left) => left.Negate();

        public static TensorExpression operator +(TensorExpression left, TensorExpression right) => left.Add(right);

        public static TensorExpression operator -(TensorExpression left, TensorExpression right) =>
            left.Subtract(right);

        public static TensorExpression operator *(TensorExpression left, TensorExpression right) =>
            left.Multiply(right);

        public static TensorExpression operator /(TensorExpression left, TensorExpression right) => left.Divide(right);

        public virtual ExpressionTree ToTree() => 
            LHSTensor == null ? new TensorExpressionVisitor(this.LinqExpression).Tree : 
            new TensorExpressionVisitor(this.LinqExpression, LHSTensor, true).Tree;

        public TensorExpression Negate() => new TensorExpression(Expression.Negate(this), GetUnaryOpShapeOrThrow());

        public TensorExpression Add(TensorExpression right) => new TensorExpression(Expression.Add(this, right,
            GetDummyBinaryMethodInfo<TensorExpression, TensorExpression>(this, right)), GetBinaryOpShapeOrThrow(right));

        public TensorExpression Subtract(TensorExpression right) => new TensorExpression(Expression.Subtract(this,
            right, GetDummyBinaryMethodInfo<TensorExpression, TensorExpression>(this, right)), GetBinaryOpShapeOrThrow(right));

        public TensorExpression Multiply(TensorExpression right) => new TensorExpression(Expression.Multiply(this,
            right, GetDummyBinaryMethodInfo<TensorExpression, TensorExpression>(this, right)), GetBinaryOpShapeOrThrow(right));

        public TensorExpression Divide(TensorExpression right) => new TensorExpression(Expression.Divide(this, right,
            GetDummyBinaryMethodInfo<TensorExpression, TensorExpression>(this, right)), GetBinaryOpShapeOrThrow(right));

        
        internal static MethodInfo GetOpMethodInfo<T>(string name, int parameters) where T : Term
        {
            MethodInfo method = typeof(TensorExpression)
                .GetMethods(BindingFlags.Static | BindingFlags.NonPublic)
                .Where(m => m.Name == name && m.GetParameters().Count() == parameters &&
                            m.GetParameters().First().ParameterType == typeof(T))
                .First();
            return method; 
        }
        protected Shape GetUnaryOpShapeOrThrow()
        {
            if (this.Shape != null)
            {
                return this.Shape;
            }
            else throw new TensorExpressionException(this, $"Could not determine the shape of unary operation with " +
                    $"operand {this.Label}.");
        }

        protected Shape GetBinaryOpShapeOrThrow(TensorExpression right)
        {
            if (this.Shape != null)
            {
                return this.Shape;
            }
            else if (right.Shape != null)
            {
                return right.Shape;
            }
            else throw new TensorExpressionException(this, $"Could not determine the shape of binary operation with " +
                    $"operands {this.Label} and {right.Label}.");
        }
        #region Dummy unary and binary methods for IAlgerba methods
        private static TensorExpression DummyUnary(Tensor l) => null;
        private static TensorExpression DummyUnary(TensorExpression l) => null;

        private static TensorExpression DummyBinary(Tensor l, Tensor r) => null;
        private static TensorExpression DummyBinary(TensorExpression l, Tensor r) => null;
        private static TensorExpression DummyBinary(Tensor l, TensorExpression r) => null;
        private static TensorExpression DummyBinary(TensorExpression l, TensorExpression r) => null;

        private static TensorExpression DummyUnary(Scalar l) => null;  
        private static TensorExpression DummyBinary(Scalar l, Scalar r) => null;
        private static TensorExpression DummyBinary(TensorExpression l, Scalar r) => null;
        private static TensorExpression DummyBinary(Scalar l, TensorExpression r) => null;

        private static TensorExpression DummyUnary(Vector l) => null;
        private static TensorExpression DummyBinary(Vector l, Vector r) => null;
        private static TensorExpression DummyBinary(Tensor l, Vector r) => null;
        private static TensorExpression DummyBinary(Vector l, Tensor r) => null;


        private static TensorExpression DummyBinary(TensorExpression l, Vector r) => null;
        private static TensorExpression DummyBinary(Vector l, TensorExpression r) => null;

        private static TensorExpression DummyUnary(Matrix l) => null;
        private static TensorExpression DummyBinary(Matrix l, Matrix r) => null;
        private static TensorExpression DummyBinary(TensorExpression l, Matrix r) => null;
        private static TensorExpression DummyBinary(Matrix l, TensorExpression r) => null;

        private static TensorExpression DummyBinary(Scalar l, Vector r) => null;
        private static TensorExpression DummyBinary(Vector l, Scalar r) => null;
        private static TensorExpression DummyBinary(Scalar l, Matrix r) => null;
        private static TensorExpression DummyBinary(Matrix l, Scalar r) => null;
        private static TensorExpression DummyBinary(Matrix l, Vector r) => null;
        private static TensorExpression DummyBinary(Vector l, Matrix r) => null;
        #endregion

    }
}