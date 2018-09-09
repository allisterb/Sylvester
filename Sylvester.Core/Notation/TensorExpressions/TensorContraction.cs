using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using System.Text;

using Sylvester.Expressions;
using Sylvester.Trees;

namespace Sylvester.Notation
{
    public class TensorContraction : TensorIndexExpression, IContractionOp
    {
        public TensorContraction(TensorIndexExpression expr, Tensor lhsTensor)
            : base(expr)
        {
            this.LHSTensor = lhsTensor;
        }

        public TensorContraction(TensorIndexExpression expr, Tensor lhsTensor, IndexSet lhsIndexSet, params Dimension[] shape)
            : base(expr, shape)
        {
            this.LHSTensor = lhsTensor;
            this.LHSIndexSet = lhsIndexSet;
        }

        public TensorContraction(MethodCallExpression expr, TensorIndexExpression tie, params Dimension[] shape) : base(expr, shape)
        {
            expr.ThrowIfNotType<TensorExpression>();
            this.LHSTensor = tie.LHSTensor;
            this.LHSIndexSet = tie.LHSIndexSet;
        }

        public TensorContraction(UnaryExpression expr, TensorIndexExpression tie, params Dimension[] shape) : base(expr, shape)
        {
            {
                expr.ThrowIfNotType<TensorExpression>();
                this.LHSTensor = tie.LHSTensor;
                this.LHSIndexSet = tie.LHSIndexSet;
            }
        }

        public TensorContraction(BinaryExpression expr, TensorIndexExpression tie, params Dimension[] shape) : base(expr, shape)
        {
            {
                expr.ThrowIfNotType<TensorExpression>();
                this.LHSTensor = tie.LHSTensor;
                this.LHSIndexSet = tie.LHSIndexSet;
            }
        }

        public override ExpressionTree ToTree() => new TensorExpressionVisitor(this.LinqExpression, (this.LHSTensor,
            this.LHSIndexSet), true).Tree;
    }
}
