using System;
using System.Collections.Generic;
using System.Linq;
using Sylvester.Notation;

namespace Sylvester.Trees
{
    public class TreeBuilderContext : TreeVisitorContext<TensorOp, OperatorNode, ValueNode>
    {
        public Stack<ITreeNode> TreeNodeStack { get; protected set; }

        public ITreeOperatorNode<TensorOp> LastTreeNodeAsOperator =>
            (TreeNodeStack.Peek() as ITreeOperatorNode<TensorOp>) ??
            throw new Exception("The current tree node is not an operator node.");

        public ITreeValueNode LastTreeNodeAsValueNode => (TreeNodeStack.Peek() as ITreeValueNode) ??
                                                         throw new Exception(
                                                             "The current tree node is not a value node.");

        public IEnumerable<Tensor> Tensors => this.TreeNodeStack.OfType<ITreeValueNode>()
            .Where(n => n.NodeType == ValueNodeType.TENSOR).Select(v => v.ValueAs<Tensor>());

        public Queue<Index> TensorIndicesQueue { get; }

        public Queue<Tensor> TensorQueue { get; }

        public ITreeOperatorNode<TensorOp> InternalNodeAsOperatorNode =>
            (InternalNode as ITreeOperatorNode<TensorOp>) ??
            throw new Exception("The current context node is not an operator node.");

        public ExpressionTree ExpressionTree { get; }

        public TreeBuilderContext(ExpressionTree tree) : base(tree)
        {
            ExpressionTree = tree;
            TreeNodeStack = new Stack<ITreeNode>();
            TreeNodeStack.Push(Tree);
            TensorQueue = new Queue<Tensor>();
            TensorIndicesQueue = new Queue<Index>();
        }

        public T LastTreeValueNodeAs<T>()
        {
            if (LastTreeNodeAsValueNode.Value is T)
            {
                return (T) LastTreeNodeAsValueNode.Value;
            }
            else
            {
                throw new InvalidOperationException($"The current tree value node is not of type {typeof(T)}.");
            }
        }

        public OperatorNode AddOperatorNode(TensorOp op)
        {
            var parent = TreeNodeStack.Count > 1 ? InternalNodeAsOperatorNode : ExpressionTree;
            var on = ExpressionTree.CreateOperatorNode(parent as OperatorNode, op);
            ExpressionTree.AddNode(on);
            TreeNodeStack.Push(on);
            return on;
        }

        public ValueNode AddValueNode(object value)
        {
            if (value is Tensor && TensorIndicesQueue.Count > 0)
            {
                Tensor t = value as Tensor;
                throw new ExpressionTreeException(ExpressionTree,
                    $"Attempting to add new value node for Tensor {t.Name} but the tensor indices queue still has " 
                + $"{TensorIndicesQueue.Count} elements and last element: {TensorIndicesQueue.Peek().Name}");
            }

            var parent = InternalNodeAsOperatorNode;
            var vn = ExpressionTree.CreateValueNode(parent as OperatorNode, value);
            ExpressionTree.AddNode(vn);
            TreeNodeStack.Push(vn);
            return vn;
        }
    }
}