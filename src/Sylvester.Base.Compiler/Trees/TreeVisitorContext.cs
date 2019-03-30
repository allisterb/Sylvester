using System;
using System.Collections.Generic;

namespace Sylvester.Trees
{
    public abstract class TreeVisitorContext<TOp, TInternal, TLeaf> : Stack<object>,
        ITreeVisitorContext<TOp, TInternal, TLeaf>
    {
        public IExpressionTree Tree { get; protected set; }

        public bool IsInternal => Count > 0 && Peek() is TInternal;

        public bool IsLeaf => Count > 0 && Peek() is TLeaf;

        public bool IsEmpty => !IsInternal && !IsLeaf;

        public TInternal InternalNode => Peek() is TInternal
            ? (TInternal) Peek()
            : throw new Exception("The last context node is not a internal node.");

        public TLeaf LeafNode => Peek() is TInternal
            ? (TLeaf) Peek()
            : throw new Exception("The last context node is not a leaf node.");

        public TreeVisitorContext(IExpressionTree tree)
        {
            Tree = tree;
        }

        public new void Push(object node)
        {
            if (node is TLeaf || node is TInternal)
            {
                base.Push(node);
            }
            else throw new Exception("The object to push is not a leaf or internal context node.");
        }

        //public new void Pop() => throw new NotSupportedException("Use the PopInternal() or PopLeaf() methods to pop context nodes.");

        public TInternal PopInternal()
        {
            if (IsInternal)
            {
                return (TInternal) Pop();
            }
            else return InternalNode;
        }

        public TLeaf PopLeaf()
        {
            if (IsLeaf)
            {
                return (TLeaf) Pop();
            }

            return LeafNode;
        }

        public ITreeVisitorContext<TOp, TInternal, TLeaf> Internal(TInternal ctx)
        {
            Push(ctx);
            return this;
        }

        public ITreeVisitorContext<TOp, TInternal, TLeaf> Leaf(TLeaf ctx)
        {
            Push(ctx);
            return this;
        }

        public void Dispose()
        {
            Pop();
        }
    }
}