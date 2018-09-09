using System.Collections.Generic;
using System.Linq.Expressions;

namespace Sylvester.Notation
{
    public static partial class Math
    {
        public static TensorExpression Square(TensorExpression l) => l * l;


        public static TensorExpression Sqrt(TensorExpression l) =>
            new TensorExpression(Expression.Call(TensorExpression.GetOpMethodInfo<TensorExpression>("Op_Sqrt", 1),
                Expression.Convert(l.LinqExpression, typeof(TensorExpression))));


        public static TensorIndexExpression Square(TensorIndexExpression l) => l * l;
    }

    public partial class TensorExpression
    {
        private static TensorExpression Op_Sqrt(TensorExpression l) => null;
    }
}