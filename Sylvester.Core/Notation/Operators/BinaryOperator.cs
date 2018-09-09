using System;
using System.Linq.Expressions;

namespace Sylvester.Notation
{
    public class BinaryOperator<TExprParam, TExprReturn> : IElementwiseOp
        where TExprParam : TensorExpression
        where TExprReturn : TensorExpression
    {
        protected Func<TExprParam, TExprParam, TExprReturn> Operation { get; }

        public BinaryOperator(Func<TExprParam, TExprParam, TExprReturn> op) : base()
        {
            Operation = op;
        }

        public TExprReturn this[TExprParam l, TExprParam r] => Operation(l, r);

        public BinaryOperator<TExprParam, TExprReturn> this[UnaryOperator<TExprParam, TExprParam> g] => 
            new BinaryOperator<TExprParam, TExprReturn>((l, r) => this[g[l], g[r]]);

       
        public static BinaryOperator<TExprParam, TExprReturn> operator |
            (BinaryOperator<TExprParam, TExprReturn> left, UnaryOperator<TExprParam, TExprParam> right) => left[right];

        public static BinaryOperator<TExprParam, TensorExpression> operator +
            (BinaryOperator<TExprParam, TExprReturn> left, BinaryOperator<TExprParam, TExprReturn> right)
        {
            return new BinaryOperator<TExprParam, TensorExpression>((l, r) =>
            {
                return new TensorExpression(Expression.Add(left[l, r], right[l, r]));
            });
        }

        public static BinaryOperator<TExprParam, TensorExpression> operator -
            (BinaryOperator<TExprParam, TExprReturn> left, BinaryOperator<TExprParam, TExprReturn> right)
        {
            return new BinaryOperator<TExprParam, TensorExpression>((l, r) =>
            {
                return new TensorExpression(left[l,r] - right[l,r]);
            });
        }

        public static BinaryOperator<TExprParam, TensorExpression> operator *
            (BinaryOperator<TExprParam, TExprReturn> left, BinaryOperator<TExprParam, TExprReturn> right)
        {
            return new BinaryOperator<TExprParam, TensorExpression>((l, r) =>
            {
                return new TensorExpression(Expression.Multiply(left[l, r], right[l, r]));
            });
        }

        public static BinaryOperator<TExprParam, TensorExpression> operator /
            (BinaryOperator<TExprParam, TExprReturn> left, BinaryOperator<TExprParam, TExprReturn> right)
        {
            return new BinaryOperator<TExprParam, TensorExpression>((l, r) =>
            {
                return new TensorExpression(Expression.Divide(left[l, r], right[l, r]));
            });
        }
    }
}