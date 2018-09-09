using System;
using System.Linq;
using System.Linq.Expressions;
using System.Reflection;

namespace Sylvester.Notation
{
#pragma warning disable CS0660

    public class Index : Term, IChildTerm, IAlgebra<Index, Index>, IComparable<Index>
    {
        public static PropertyInfo OrderInfo { get; } = typeof(Index).GetProperty("Order");

        public IndexType Type { get; protected set; }

        public ITerm Parent => Set;

        public IndexSet Set { get; internal set; }

        public int Order { get; internal set; }

        protected Expression IndexExpression { get; set; }

        internal override Expression LinqExpression =>
            Type == IndexType.Dimension ? Expression.Constant(this) : IndexExpression;

        internal override Type ExpressionType { get; } = typeof(Index);

        internal override Name DefaultNameBase { get; } = "i";


        public Index(IndexSet set, int order, int dim, string name) : base(name)
        {
            Set = set;
            Order = order;
            Type = IndexType.Dimension;
        }

        public Index(IndexSet set, int order, Expression expr) : base(GetNameFromLinqExpression(expr))
        {
            Set = set;
            Order = order;
            IndexExpression = expr;
            Type = IndexType.Expression;
        }

        public Index(int i) : base(GetNameFromLinqExpression(Expression.Constant(i)))
        {
            IndexExpression = Expression.Constant(i);
            Type = IndexType.Literal;
        }
        

        public static implicit operator Int32(Index i) => i.Order;

        public static implicit operator Index(Int32 i) => new Index(i);

        public static Index operator - (Index left) => left.Negate();
        
        public static Index operator + (Index left, Index right)
            => left.Add(right);

        public static Index operator - (Index left, Index right)
            => left.Subtract(right);

        public static Index operator * (Index left, Index right)
            => left.Multiply(right);

        public static Index operator / (Index left, Index right)
            => left.Divide(right);

        public Index Negate() => new Index(this.Set, this.Order, Expression.Negate(this));

        public Index Add(Index right)
            => new Index(this.Set, this.Order, Expression.Add(this, right, GetDummyBinaryMethodInfo(this, right)));

        public Index Subtract(Index right)
            => new Index(this.Set, this.Order, Expression.Subtract(this, right, GetDummyBinaryMethodInfo(this, right)));

        public Index Multiply(Index right)
            => new Index(this.Set, this.Order, Expression.Multiply(this, right, GetDummyBinaryMethodInfo(this, right)));

        public Index Divide(Index right)
            => new Index(this.Set, this.Order, Expression.Divide(this, right, GetDummyBinaryMethodInfo(this, right)));

        public int CompareTo(Index i)
        {
            return Order.CompareTo(i.Order);
        }

        private static MethodInfo GetDummyBinaryMethodInfo(Index l, Index r)
        {
            var method = typeof(Index).GetMethods(BindingFlags.NonPublic | BindingFlags.Static)
                .Where(m => m.Name == "DummyBinary" && m.GetParameters()[0].ParameterType == l.LinqExpression.Type
                                                    && m.GetParameters()[1].ParameterType == r.LinqExpression.Type).First();
            return method;
        }

        private static Index DummyBinary(Index l, Index r) => null;
        private static Index DummyBinary(Int32 l, Index r) => null;
        private static Index DummyBinary(Index l, Int32 r) => null;
        private static Index DummyBinary(Int32 l, Int32 r) => null;
    }

#pragma warning restore CS0660
}