using System;

namespace Sylvester.Notation
{
    public class ContractionOperator : IContractionOp
    {
        protected readonly Func<TensorIndexExpression, TensorIndexExpression> Operation;

        public ContractionOperator(Func<TensorIndexExpression, TensorIndexExpression> op)
        {
            Operation = op;
        }

        public TensorIndexExpression this[TensorIndexExpression e] => Operation(e);

        public TensorIndexExpression this[TensorExpression e] => Operation(new TensorIndexExpression(e));

        public TensorIndexExpression this[Tensor t] => Operation((TensorIndexExpression) t);
    }
}