using System;
using System.Linq;
using System.Linq.Expressions;
using Sylvester.Expressions;
using Sylvester.Notation;

namespace Sylvester.Trees
{
    public class TensorExpressionVisitor : ExpressionVisitor
    {
        public ExpressionTree Tree { get; set; }

        internal Expression LinqExpression;

        internal TreeBuilderContext Context { get; set; }


        public TensorExpressionVisitor(Expression expr, bool visit = true)
        {
            LinqExpression = expr;
            Tree = new ExpressionTree();
            Context = new TreeBuilderContext(Tree);
            if (visit)
            {
                Visit();
            }
        }

        public TensorExpressionVisitor(Expression expr, Tensor lhs, bool visit = true)
        {
            LinqExpression = expr;
            Tree = new ExpressionTree(lhs);
            Context = new TreeBuilderContext(Tree);
            if (visit)
            {
                Visit();
            }
        }

        public TensorExpressionVisitor(Expression expr, (Tensor tensor, IndexSet indices) lhs, bool visit = true)
        {
            LinqExpression = expr;
            Tree = new ExpressionTree(lhs.tensor, lhs.indices);
            Context = new TreeBuilderContext(Tree);
            if (visit)
            {
                Visit();
            }
        }


        public void Visit()
        {
            Visit(LinqExpression);
        }

        protected override Expression VisitConstant(ConstantExpression node)
        {
            base.VisitConstant(node);
            Tensor t;
            if (node.Value is Array)
            {
                t = node.FlattenConstantExpressionArrayValue<Tensor>();
            }
            else if (node.Value is Tensor)
            {
                t = node.Value as Tensor;
            }
            else
                throw new InvalidOperationException($"Can't convert ConstantExpression" +
                                                    $"of type {node.Type.Name} to type Tensor.");

            if (Context.InternalNode.Op == TensorOp.Index)
            {
                Context.TensorQueue.Enqueue(t);
            }
            if (!t.IsDefined)
            {
                Context.AddValueNode(t);
            }
            else if (t.IsElementwiseDefined)
            {
                var on = Context.AddOperatorNode(TensorOp.ElementWiseAssign);
                using (Context.Internal(on))
                {
                    Context.AddValueNode(t);
                    base.Visit(t.ElementwiseDefinition.LinqExpression);
                }
            }
            else if (t.IsContractionDefined)
            {
                var on = Context.AddOperatorNode(TensorOp.IndexedAssign);
                using (Context.Internal(on))
                {
                    Context.AddValueNode(t);
                    base.Visit(t.ContractionDefinition.Expression.LinqExpression);
                }
            }
            return node;
        }
                 
        protected override Expression VisitIndex(IndexExpression node)
        {
            var on = Context.AddOperatorNode(TensorOp.Index);
            using (Context.Internal(on))
            {
                base.VisitIndex(node);
                var t = Context.TensorQueue.Dequeue();
                if (t.Dimensions.Length < Context.TensorIndicesQueue.Count)
                {
                    throw new ExpressionVisitorException(this, node,
                        $"Tensor {t.Name.Label} has {t.Dimensions.Length} dimensions but the tensor indices queue has length {Context.TensorIndicesQueue.Count}.");
                }

                var indices = new Index[Context.TensorIndicesQueue.Count];
                for (var i = 0; i < indices.Length; i++)
                {
                    indices[i] = Context.TensorIndicesQueue.Dequeue();
                }

                var set = new IndexSet(t, indices);
                Context.AddValueNode(set);
            }

            return node;
        }

        protected override Expression VisitParameter(ParameterExpression node)
        {
            base.VisitParameter(node);

            
                var t = Context.TensorQueue.First();
                var i = Context.TensorIndicesQueue.Count;
                if (!Term.Terms.ContainsKey(node.Name))
                {
                    throw new InvalidOperationException($"The term table does not contain a term with id {node.Name}.");
                }
                else
                    Context.TensorIndicesQueue.Enqueue((Index)Term.Terms[node.Name]);
            
            return node;
        }

        protected override Expression VisitUnary(UnaryExpression node)
        {
            if (node.NodeType == ExpressionType.Convert)
            {
                base.Visit(node.Operand);
                return node;
            }

            using (Context.Internal(Context.AddOperatorNode(node.NodeType.ToOp())))
            {
                base.VisitUnary(node);
            }
            return node;
        }

        protected override Expression VisitBinary(BinaryExpression node)
        {
            using (Context.Internal(Context.AddOperatorNode(node.NodeType.ToOp())))
            {
                base.VisitBinary(node);
            }
            return node;
        }

        protected override Expression VisitMethodCall(MethodCallExpression node)
        {
            using (Context.Internal(Context.AddOperatorNode(node.MethodCallToTensorOp())))
            {
                base.VisitMethodCall(node);
            }
            return node;
        }

        protected override Expression VisitRuntimeVariables(RuntimeVariablesExpression node)
        {
            var v = node.Variables.Single();
            return node;
            //Term.Terms[v.Name]
        }
    }
}