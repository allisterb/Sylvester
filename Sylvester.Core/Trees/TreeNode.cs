using System;
using Sawmill;

namespace Sylvester.Trees
{
    public abstract class TreeNode : ITreeNode
    {
        public int Id { get; internal set; }

        public int? ParentId { get; internal set; }

        public ITreeNode Parent { get; internal set; }

        public TreeNodePosition Position { get; internal set; }

        public abstract string Label { get; }

        public ITreeNode Left { get; internal set; }

        public ITreeNode Right { get; internal set; }

        public bool IsOperator => this is OperatorNode;

        public bool IsValue => this is ValueNode;

        public bool HasLeft => this is OperatorNode && (this as OperatorNode).Left != null;

        public bool HasRight => this is OperatorNode && (this as OperatorNode).Right != null;

        protected TreeNode(int id, int? parentId, TreeNodePosition pos)
        {
            Id = id;
            ParentId = parentId;
            Position = pos;
        }

        public abstract Children<ITreeNode> GetChildren();

        public abstract ITreeNode SetChildren(Children<ITreeNode> newChildren);

        public ITreeNode RewriteChildren(Func<ITreeNode, ITreeNode> transformer)
            => this.DefaultRewriteChildren(transformer);

        public bool Equals(ITreeNode other) => Id == other.Id;
    }
}