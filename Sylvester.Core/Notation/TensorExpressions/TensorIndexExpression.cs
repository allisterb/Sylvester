using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using System.Text;

using Sylvester.Trees;
using Sylvester.Expressions;

namespace Sylvester.Notation
{
    public class TensorIndexExpression : TensorExpression, IAlgebra<TensorIndexExpression, TensorIndexExpression>
    {
        public IndexSet IndexSet { get; protected set; }

        public Shape ExpressionShape { get; protected set; }

        internal TensorIndexExpression(TensorExpression expr) : base(expr.LinqExpression)
        {
            if (!(expr.LinqExpression is UnaryExpression || expr.LinqExpression is BinaryExpression
                || expr.LinqExpression is MethodCallExpression))
            {
                throw new ArgumentException("This tensor expression cannot be used as a tensor index expression");
            }
            ExpressionShape = expr.Shape;

        }

        internal TensorIndexExpression(IndexExpression expr, IndexSet set, Dimension[] dim) : base(expr, dim)
        {
            expr.ThrowIfNotType<Tensor>();
            if (!(expr.Object is ConstantExpression))
            {
                throw new ArgumentException("This Linq expression cannot be used as a tensor index expression.");
            }
            IndexSet = set;
        }

        internal TensorIndexExpression(MethodCallExpression expr, params Dimension[] dim) : base(expr, dim) {}


        internal TensorIndexExpression(UnaryExpression expr, params Dimension[] dim) : base(expr, dim)
        {
            expr.ThrowIfNotType<TensorExpression>();
            expr.Operand.ThrowIfNotType<TensorIndexExpression>();
        }

        internal TensorIndexExpression(BinaryExpression expr, params Dimension[] dim) : base(expr, dim)
        {
            expr.ThrowIfNotType<TensorIndexExpression>();
        }
        
        internal TensorIndexExpression(TensorIndexExpression tie) : base(tie.LinqExpression, new Dimension[0]) {}

        internal TensorIndexExpression(TensorIndexExpression c, params Dimension[] dim) : base(c.LinqExpression, dim)
        { }

        internal TensorIndexExpression(TensorIndexExpression c, Tensor lhsTensor, params Dimension[] dim) :
            base(c.LinqExpression, lhsTensor, dim)
        { }


        public TensorIndexExpression this[Dimension n] => new TensorIndexExpression(this, n);
  
        public static TensorIndexExpression operator +(TensorIndexExpression left, TensorIndexExpression right) =>
           new TensorIndexExpression(Expression.Add(left, right,
               GetDummyBinaryMethodInfo<TensorIndexExpression, TensorIndexExpression>(left, right)));

        public static TensorIndexExpression operator -(TensorIndexExpression left, TensorIndexExpression right) =>
           new TensorIndexExpression(Expression.Subtract(left, right,
               GetDummyBinaryMethodInfo<TensorIndexExpression, TensorIndexExpression>(left, right)));

        public static TensorIndexExpression operator *(TensorIndexExpression left, TensorIndexExpression right) =>
            new TensorIndexExpression(Expression.Multiply(left, right, 
                GetDummyBinaryMethodInfo<TensorIndexExpression, TensorIndexExpression>(left, right)));

        public static TensorIndexExpression operator *(TensorIndexExpression left, TensorExpression right) =>
            new TensorIndexExpression(Expression.Multiply(left, right,
                GetDummyBinaryMethodInfo<TensorIndexExpression, TensorIndexExpression>(left, right)));

        public static TensorIndexExpression operator *(TensorExpression left, TensorIndexExpression right) =>
           new TensorIndexExpression(Expression.Multiply(right, left,
               GetDummyBinaryMethodInfo<TensorIndexExpression, TensorIndexExpression>(right, left)));

        public static TensorIndexExpression operator /(TensorIndexExpression left, TensorIndexExpression right) =>
            new TensorIndexExpression(Expression.Divide(left, right,
                GetDummyBinaryMethodInfo<TensorIndexExpression, TensorIndexExpression>(left, right)));

        public static TensorIndexExpression operator /(TensorIndexExpression left, TensorExpression right) =>
            new TensorIndexExpression(Expression.Divide(left, right,
                GetDummyBinaryMethodInfo<TensorIndexExpression, TensorIndexExpression>(left, right)));


        public new TensorIndexExpression Negate() => new TensorIndexExpression(Expression.Negate(this, 
            GetDummyUnaryMethodInfo<TensorIndexExpression, TensorIndexExpression>(this)));

        public TensorIndexExpression Add(TensorIndexExpression right) => new TensorIndexExpression(Expression.Add(this, right,
            GetDummyBinaryMethodInfo<TensorIndexExpression, TensorIndexExpression>(this, right)));

        public TensorIndexExpression Subtract(TensorIndexExpression right) => new TensorIndexExpression(Expression.Subtract(this,
            right, GetDummyBinaryMethodInfo<TensorIndexExpression, TensorIndexExpression>(this, right)));

        public TensorIndexExpression Multiply(TensorIndexExpression right) => new TensorIndexExpression(Expression.Multiply(this,
            right, GetDummyBinaryMethodInfo<TensorIndexExpression, TensorIndexExpression>(this, right)));

        public TensorIndexExpression Divide(TensorIndexExpression right) => new TensorIndexExpression(Expression.Divide(this, right,
            GetDummyBinaryMethodInfo<TensorIndexExpression, TensorIndexExpression>(this, right)));

        public TensorExpression GetDimensionProductExpression()
        {
            Shape exprShape = this.IndexSet != null ? this.IndexSet.Tensor.Shape : this.ExpressionShape;  
            TensorExpression mulExpr = exprShape.Count() > 1 ?
               (Scalar)exprShape[0] * (Scalar)exprShape[1] : (Scalar)exprShape[0];
            for (int i = 2; i < exprShape.Count(); i++)
            {
                mulExpr = mulExpr * (Scalar)exprShape[i];
            }
            return mulExpr;
        }

        private static TensorIndexExpression DummyUnary(Tensor l) => null;
        private static TensorIndexExpression DummyBinary(Tensor l, Tensor r) => null;
        private static TensorIndexExpression DummyBinary(TensorIndexExpression l, Tensor r) => null;
        private static TensorIndexExpression DummyBinary(TensorIndexExpression l, TensorIndexExpression r) => null;
        private static TensorIndexExpression DummyBinary(TensorIndexExpression l, TensorExpression r) => null;
        private static TensorIndexExpression DummyBinary(TensorIndexExpression l, Scalar r) => null;
        private static TensorIndexExpression DummyBinary(Scalar l, TensorIndexExpression r) => null;
    }
}
