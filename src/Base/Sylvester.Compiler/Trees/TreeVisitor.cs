namespace Sylvester.Trees
{
    public abstract class TreeVisitor<TOp, TInternalContext, TLeafContext> : ITreeVisitor<TOp>
        where TInternalContext : class where TLeafContext : class
    {
        public IExpressionTree Tree { get; set; }

        public TreeVisitorContext<TOp, TInternalContext, TLeafContext> Context { get; protected set; }

        public TreeVisitor(IExpressionTree tree, bool visit = true)
        {
            Tree = tree;
            if (visit)
            {
                VisitTree();
            }
        }

        public void Visit(ITreeNode tn)
        {
            if (tn is ITreeOperatorNode<TOp>)
            {
                VisitInternal(tn as ITreeOperatorNode<TOp>);
            }
            else
            {
                VisitLeaf(tn as ITreeValueNode);
            }
        }

        public void VisitTree()
        {
            Visit(Tree);
            AfterVisitTree();
        }

        public abstract void VisitLeaf(ITreeValueNode node);

        public virtual void VisitInternal(ITreeOperatorNode<TOp> on)
        {
            if (on.Left != null)
            {
                Visit(on.Left);
            }

            if (on.Right != null)
            {
                Visit(on.Right);
            }
        }

        public abstract void AfterVisitTree();
    }
}