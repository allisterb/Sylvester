using System;
using System.Collections.Generic;
using System.Linq.Expressions;

namespace Sylvester.Expressions
{
    public class ExpressionVisitorException : Exception
    {
        public ExpressionVisitor ExpressionVisitor { get; protected set; }

        public Expression Expression { get; protected set; }

        public ExpressionVisitorException(ExpressionVisitor visitor, Expression expr, string message) : base(message)
        {
            ExpressionVisitor = visitor;
            Expression = expr;
        }
    }
}
