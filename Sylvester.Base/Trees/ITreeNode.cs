using System;
using Sawmill;

namespace Sylvester.Trees
{
    public interface ITreeNode : IEquatable<ITreeNode>, IRewritable<ITreeNode>
    {
        int Id { get; }

        int? ParentId { get; }

        TreeNodePosition Position { get; }

        ITreeNode Parent { get; }

        ITreeNode Left { get; }

        ITreeNode Right { get; }

        string Label { get; }

        bool IsOperator { get; }

        bool IsValue { get; }

        bool HasLeft { get; }

        bool HasRight { get; }
    }
}