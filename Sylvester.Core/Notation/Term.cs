using System;
using System.Collections.Generic;
using System.Collections.Concurrent;
using System.Linq;
using System.Linq.Expressions;
using System.Reflection;

namespace Sylvester.Notation
{
    /// <summary>
    /// Abstracts a notation term.
    /// </summary>
    public abstract class Term : ITerm
    {
        public static ConcurrentDictionary<string, Term> Terms { get; }

        public string Id { get; protected set; }

        public Name Name { get; protected set; }

        public string Label => Name.Label;

        internal abstract Name DefaultNameBase { get; }

        internal abstract Expression LinqExpression { get; }

        internal virtual Type ExpressionType => LinqExpression.Type;

        private static readonly int A = 'A';
        private static readonly int a = 'a';
        private static readonly int Z = 'Z';
        private static readonly int z = 'z';

        static Term()
        {
            Terms = new ConcurrentDictionary<string, Term>();
        }

        internal Term(string name)
        {
            Id = Guid.NewGuid().ToString("N");
            Name = name;
            if (!Terms.TryAdd(Id, this))
            {
                throw new Exception($"Could not add term with Id {Id} to term table.");
            }
        }

        internal Term(char name) : this(new string(name, 1)) {}

        internal Term(Term t)
        {
            Id = t.Id;
        }


        public static implicit operator Expression(Term e)
        {
            return e.LinqExpression;
        }

        protected string GenerateName(int index, string indexNameBase)
        {
            indexNameBase = indexNameBase != string.Empty ? indexNameBase : (string) DefaultNameBase;

            if (indexNameBase.Length == 1)
            {
                var c = indexNameBase.First();
                var n = c + index;
                var lower = Char.IsLower(c) ? a : A;
                var upper = Char.IsLower(c) ? z : Z;

                if (n < lower || n > upper)
                {
                    throw new ArgumentException(
                        "Auto-generated name past last letter of alphabet. Consider using a numeric name base like a0.");
                }

                return new string(Convert.ToChar(n), 1);
            }
            else if (indexNameBase.Length == 2 && Char.IsLetter(indexNameBase[0]) && Char.IsNumber(indexNameBase[1]))
            {
                var i = Int32.Parse(indexNameBase.Substring(1, 1)) + index;
                return new string(indexNameBase[0], 1) + i;
            }
            else throw new ArgumentException($"Unknown name base {indexNameBase}");
        }


        protected static string GetNameFromLinqExpression(Expression expr)
        {
            switch (expr)
            {
                case ConstantExpression ce:
                    switch (ce.Value)
                    {
                        case int i: return i.ToString();
                        case Term t: return t.Name;
                        case Array a: return a.Flatten<Tensor>().First().Name;
                        default: throw new ArgumentException($"Unknown constant expression value type: {ce.Value.GetType()}.");
                    }

                case BinaryExpression be:
                    return be.NodeType + "_" + GetNameFromLinqExpression(be.Left)
                           + "_" + GetNameFromLinqExpression(be.Right);

                case ParameterExpression pe: return pe.Name;

                case IndexExpression ie:
                    return "Index" + "_" + GetNameFromLinqExpression(ie.Object) + "_" + ie.Arguments
                               .Select(GetNameFromLinqExpression)
                               .Aggregate((s1, s2) => s1 + "_" + s2);

                case MethodCallExpression me:
                    return me.Method.Name;

                default:
                    throw new ArgumentException($"Unknown expression type: {expr.NodeType.ToString()} " +
                        $"{expr.GetType().Name}.");
            }
        }

        internal static MethodInfo GetDummyUnaryMethodInfo<TType, TReturn>(Term l) 
            where TType: Term where TReturn : Term
        {
            Type lt = l.ExpressionType;

            MethodInfo method = typeof(TType).GetMethods(BindingFlags.NonPublic | BindingFlags.Static)
                .Where(m => m.Name == "DummyUnary" && m.GetParameters()[0].ParameterType == lt
                                                   && m.ReturnType == typeof(TReturn)).First();
            return method;
        }

        internal static MethodInfo GetDummyBinaryMethodInfo<TType, TReturn>(Term l, Term r)
            where TType : Term where TReturn : Term
        {
            Type lt = l.ExpressionType;
            Type rt = r.ExpressionType;


            MethodInfo method = typeof(TType).GetMethods(BindingFlags.NonPublic | BindingFlags.Static)
                .Where(m => m.Name == "DummyBinary" && m.GetParameters()[0].ParameterType == lt
                                                    && m.GetParameters()[1].ParameterType == rt
                                                    && m.ReturnType == typeof(TReturn)).First();
            return method;
        }
    }
}