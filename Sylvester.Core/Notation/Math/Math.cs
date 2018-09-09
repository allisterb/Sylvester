using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using System.Text;

namespace Sylvester.Notation
{
    public static partial class Math
    {
        private static MethodCallExpression GetOpMethodCall(string op, params TensorIndexExpression[] args)
        {
            return Expression.Call(TensorExpression.GetOpMethodInfo<TensorIndexExpression>("Op_" + op, args.Length),
                args.Select(a => Expression.Convert(a, typeof(TensorIndexExpression))).ToArray());
        }
    }
}
