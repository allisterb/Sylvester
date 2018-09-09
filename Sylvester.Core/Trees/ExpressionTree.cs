using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using Sawmill;
using Sylvester.Notation;

namespace Sylvester.Trees
{
    public class ExpressionTree : OperatorNode, IExpressionTree, IEqualityComparer<ITreeNode>,
        IEqualityComparer<ITreeValueNode>
    {
        public Expression LinqExpression { get; protected set; }

        public Tensor OutputTensor => OutputNode.ValueAs<Tensor>();

        public int Count => HashSet.Count;

        public List<OperatorNode> OperatorNodes => Root.SelfAndDescendants().OfType<OperatorNode>().ToList();

        public List<ValueNode> ValueNodes => Root.SelfAndDescendants().OfType<ValueNode>().ToList();

        public ITreeNode Root => this;

        public IEnumerable<ITreeNode> Children => HashSet.Cast<ITreeNode>();

        public ITreeValueNode OutputNode
        {
            get
            {
                if (Root.Left is ITreeValueNode)
                {
                    return Root.Left as ITreeValueNode;
                }
                else if ((Root.Left is OperatorNode) && (Root.Left as OperatorNode).Op == TensorOp.Index)
                {
                    return Left.Left as ITreeValueNode;
                }
                else throw new ExpressionTreeException(this, "The tree's output node cannnot be determined.");
            }
        }

        public IEnumerable<ITreeValueNode> TensorNodes => ValueNodes.Where(vn => vn.NodeType == ValueNodeType.TENSOR);

        public IEnumerable<ITreeValueNode> IndexSetNodes =>
            ValueNodes.Where(vn => vn.NodeType == ValueNodeType.INDEXSET);

        public IEnumerable<ITreeValueNode> DefinedVariableNodes =>
            OperatorNodes
                .Where(on => on.Op == TensorOp.ElementWiseAssign)
                .Select(on => on.Left)
                .Cast<ITreeValueNode>()
                .Distinct(this);

        public IEnumerable<ITreeValueNode> TensorDimensionNodes => ValueNodes.Where(vn => vn.Value is Dimension);
        
        public IEnumerable<ITreeValueNode> TensorDimensionAsScalarNodes
        {
            get
            {
                List<string> names = TensorNodes.Distinct(this).Select(n => n.Label).ToList();
                List<ITreeValueNode> nodes = new List<ITreeValueNode>();
                foreach(ITreeValueNode n in TensorNodes.Distinct(this))
                {
                    if (n.Value is Scalar s && s.Label.Contains("DIM") && Char.IsDigit(s.Label.Last()))
                    {
                        string prefix = s.Label.Remove(s.Label.IndexOf("DIM"));
                        if (names.Contains(prefix))
                        {
                            nodes.Add(n);
                        }
                    }
                }
                return nodes;
            }
        }
        public IEnumerable<ITreeValueNode> InputVariableNodes =>
            TensorNodes.Distinct(this)
            .Except(TensorDimensionNodes)
            .Except(DefinedVariableNodes)
            .Except(TensorDimensionAsScalarNodes)
            .Where(n => n != OutputNode);

        protected HashSet<TreeNode> HashSet { get; } = new HashSet<TreeNode>();


        public ExpressionTree() : base(0, null, TreeNodePosition.RIGHT, TensorOp.Assign)
        {
            HashSet.Add(this);
            AddNode(CreateValueNode(this, null));
        }

        public ExpressionTree(Term term) : this()
        {
            LinqExpression = term.LinqExpression;
        }

        public ExpressionTree(Tensor lhsTensor) : base(0, null, TreeNodePosition.RIGHT, TensorOp.Assign)
        {
            HashSet.Add(this);
            AddNode(CreateValueNode(this, lhsTensor));
        }

        public ExpressionTree(Tensor lhsTensor, IndexSet lhsTensorIndices) : base(0, null, TreeNodePosition.RIGHT,
            TensorOp.IndexedAssign)
        {
            HashSet.Add(this);
            var n = AddNode(CreateOperatorNode(this, TensorOp.Index)) as OperatorNode;
            AddNode(CreateValueNode(n, lhsTensor));
            AddNode(CreateValueNode(n, lhsTensorIndices));
        }

        public OperatorNode CreateOperatorNode(OperatorNode parent, TensorOp op)
        {
            var pos = parent.HasLeft ? TreeNodePosition.RIGHT : TreeNodePosition.LEFT;
            var nid = pos == TreeNodePosition.LEFT ? CountChildren(parent) + 1 : CountChildren(parent) + 2;
            var node = new OperatorNode(nid, parent.Id, pos, op)
            {
                Parent = parent
            };
            return node;
        }

        public ValueNode CreateValueNode(OperatorNode parent, object value)
        {
            var pos = parent.HasLeft ? TreeNodePosition.RIGHT : TreeNodePosition.LEFT;
            var nid = pos == TreeNodePosition.LEFT ? CountChildren(parent) + 1 : CountChildren(parent) + 2;
            var node = new ValueNode(nid, parent.Id, pos, value)
            {
                Parent = parent
            };
            return node;
        }

        public ITreeNode AddNode(ITreeNode n)
        {
            TreeNode parent, node;
            if (n is TreeNode treeNode)
            {
                node = treeNode;
            }
            else
            {
                throw new ExpressionTreeException(this, n, $"Argument to AddNode is not of type TreeNode.");
            }

            if (n.Parent is TreeNode parentNode)
            {
                parent = parentNode;
            }
            else
            {
                throw new ExpressionTreeException(this, n, $"Argument AddNode's parent is not of type TreeNode.");
            }

            if (node.Position == TreeNodePosition.LEFT)
            {
                if (parent.HasLeft)
                {
                    throw new ExpressionTreeException(this, node, $"Parent tree node with id {parent.Id} already has a left child.");
                }

                parent.Left = node;
            }
            else
            {
                if (parent.HasRight)
                {
                    throw new ExpressionTreeException(this, node, $"Parent tree node with id {parent.Id} already has a right child.");
                }

                parent.Right = node;
            }

            HashSet.Add(node);
            return node;
        }

        public ValueNode ValueNodeAtIndex(int index) => (ValueNode)HashSet.ElementAt(index);

        public OperatorNode OperatorNodeAtIndex(int index) => (OperatorNode)HashSet.ElementAt(index);

        public int CountChildren(TreeNode node)
        {
            int Count(int start, ITreeNode tn)
            {
                if (tn is null || tn is ValueNode)
                {
                    return start;
                }
                else if (tn is OperatorNode on)
                {
                    var lcount = on.Left != null ? Count(start + 1, on.Left) : start + 1;
                    var rcount = on.Right != null ? Count(lcount + 1, on.Right) : lcount;
                    return rcount;
                }
                else throw new ExpressionTreeException(this, node, $"Unknown tree node type: {node.GetType().Name}.");
            }

            return Count(0, node);
        }

        public int CountChildren() => CountChildren(this);

        public bool TreeNodeIsDimensionVariable(ITreeNode node)
        {
            if (node is ITreeValueNode vn)
            {
                List<string> names = TensorNodes.Distinct(this).Select(n => n.Label).ToList();
                if (vn.Value is Scalar s && s.Label.Contains("DIM") && Char.IsDigit(s.Label.Last()))
                {
                    string prefix = s.Label.Remove(s.Label.IndexOf("DIM"));
                    if (names.Contains(prefix))
                    {
                        return true;
                    }
                    else
                    {
                        return false;
                    }
                }
                else
                {
                    return false;
                }
            }
            else
            {
                return false;
            }
        }

        public int GetHashCode(ITreeNode node)
        {
            return (node.Label).GetHashCode();
        }

        public bool Equals(ITreeNode left, ITreeNode right)
        {
            return (left.Label).Equals(right.Label);
        }

        public int GetHashCode(ITreeValueNode node)
        {
            return (node.Label).GetHashCode();
        }

        public bool Equals(ITreeValueNode left, ITreeValueNode right)
        {
            return (left.Label).Equals(right.Label);
        }
    }
}