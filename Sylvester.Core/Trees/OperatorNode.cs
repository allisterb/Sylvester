using Sawmill;

namespace Sylvester.Trees
{
    public class OperatorNode : TreeNode, ITreeOperatorNode<TensorOp>
    {
        public TensorOp Op { get; protected set; }

        public override string Label => Op.ToString();

        internal OperatorNode(int id, int? parentId, TreeNodePosition pos, TensorOp op) : base(id, parentId, pos)
        {
            Op = op;
        }

        public override Children<ITreeNode> GetChildren() => HasRight ? Children.Two(Left, Right) : Children.One(Left);

        public override ITreeNode SetChildren(Children<ITreeNode> newChildren)
        {
            Left = newChildren.First;
            Right = newChildren.Second;
            return this;
        }
    }
}