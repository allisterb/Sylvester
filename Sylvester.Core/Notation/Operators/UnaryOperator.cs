using System;
using System.Linq.Expressions;

namespace Sylvester.Notation
{
    public class UnaryOperator<TExprParam, TExprReturn> : IElementwiseOp
        where TExprParam : TensorExpression
        where TExprReturn : TensorExpression
    {
        protected Func<TExprParam, TExprReturn> Operation { get; set; }


        public UnaryOperator(Func<TExprParam, TExprReturn> op)
        {
            Operation = op;
        }

        public TExprReturn this[TExprParam e]
        {
            get
            {
                return Operation(e);
            }
        }

        public UnaryOperator<TExprParam, TExprReturn> this[UnaryOperator<TExprParam, TExprParam> right] => 
            new UnaryOperator<TExprParam, TExprReturn>((e) => this[right[e]]);

        public BinaryOperator<TExprParam, TensorExpression> this[BinaryOperator<TExprParam, TExprReturn> right] => 
            new BinaryOperator<TExprParam, TensorExpression>((l, r) => new TensorExpression(right[l,r]));

        public static UnaryOperator<TExprParam, TExprReturn> operator |(UnaryOperator<TExprParam, TExprReturn> left, 
            UnaryOperator<TExprReturn, TExprReturn> right)
            => new UnaryOperator<TExprParam, TExprReturn>((e) => right[left[e]]);

        public static UnaryOperator<TExprParam, TensorExpression> operator +(UnaryOperator<TExprParam, TExprReturn> left, 
            UnaryOperator<TExprParam, TExprReturn> right)
        {
            return new UnaryOperator<TExprParam, TensorExpression>((e) => new TensorExpression(left[e] + right[e]));
        }

        public static UnaryOperator<TExprParam, TensorExpression> operator -(UnaryOperator<TExprParam, TExprReturn> left,
           UnaryOperator<TExprParam, TExprReturn> right)
        {
            return new UnaryOperator<TExprParam, TensorExpression>((e) => new TensorExpression(left[e] - right[e]));
        }

        public static UnaryOperator<TExprParam, TensorExpression> operator *(UnaryOperator<TExprParam, TExprReturn> left,
           UnaryOperator<TExprParam, TExprReturn> right)
        {
            return new UnaryOperator<TExprParam, TensorExpression>((e) => new TensorExpression(left[e] * right[e]));
        }

        public static UnaryOperator<TExprParam, TensorExpression> operator /(UnaryOperator<TExprParam, TExprReturn> left,
                  UnaryOperator<TExprParam, TExprReturn> right)
        {
            return new UnaryOperator<TExprParam, TensorExpression>((e) => new TensorExpression(left[e] / right[e]));
        }
    }
}