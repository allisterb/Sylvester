using System;
using System.Collections.Generic;
using System.Text;

namespace Sylvester.Trees
{
    public class ExpressionTreeException : Exception
    {
        public ExpressionTree ExpressionTree { get; protected set; }

        public ITreeNode TreeNode { get; protected set; } 

        public ExpressionTreeException(ExpressionTree tree, string message) : base(message)
        {
            ExpressionTree = tree;
        }

        public ExpressionTreeException(ExpressionTree tree, ITreeNode node, string message) : this(tree, message)
        {
            ExpressionTree = tree;
            TreeNode = node;
        }
    }
}
