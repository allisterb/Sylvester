using System;

namespace Sylvester.Trees
{
    public interface ITreeVisitorContext<TOp, TInternal, TLeaf> : IDisposable
    {
        IExpressionTree Tree { get; }

        TInternal InternalNode { get; }

        TLeaf LeafNode { get; }

        void Push(object node);

        TInternal PopInternal();

        TLeaf PopLeaf();

        ITreeVisitorContext<TOp, TInternal, TLeaf> Internal(TInternal node);

        ITreeVisitorContext<TOp, TInternal, TLeaf> Leaf(TLeaf node);
    }
}